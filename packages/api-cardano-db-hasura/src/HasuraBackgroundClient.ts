import { exec } from 'child_process'
import util, { ModuleState } from '@cardano-graphql/util'
import { GraphQLSchema } from 'graphql'
import { GraphQLClient, gql } from 'graphql-request'
import pRetry from 'p-retry'
import path from 'path'
import { dummyLogger, Logger } from 'ts-log'
import { Asset, Block } from './graphql_types'
import { AssetMetadataAndHash, AssetMetadataHashAndId, AssetWithoutTokens } from './typeAliases'
import { Schema } from '@cardano-ogmios/client'

const epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.'

const withHexPrefix = (value: string) => `\\x${value !== undefined ? value : ''}`

export class HasuraBackgroundClient {
  private client: GraphQLClient
  private applyingSchemaAndMetadata: boolean
  private state: ModuleState
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraCliExtPath: string,
    readonly hasuraUri: string,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.applyingSchemaAndMetadata = false
    this.client = new GraphQLClient(
      `${this.hasuraUri}/v1/graphql`,
      {
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      }
    )
  }

  private async hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `${this.hasuraCliPath} --cli-ext-path ${this.hasuraCliExtPath} --skip-update-check --project ${path.resolve(__dirname, '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          if (stdout !== '') this.logger.debug({ module: 'HasuraBackgroundClient' }, stdout)
          resolve()
        }
      )
    })
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initializing')
    await this.applySchemaAndMetadata()
    this.logger.debug({ module: 'HasuraBackgroundClient' }, 'graphql-engine setup')
    await pRetry(async () => {
      const result = await this.client.request(
        gql`query {
            epochs (limit: 1, order_by: { number: desc }) {
                number
            }
        }`
      )
      if (result.epochs.length === 0) {
        this.logger.debug({ module: 'HasuraBackgroundClient' }, epochInformationNotYetAvailable)
        throw new Error(epochInformationNotYetAvailable)
      }
    }, {
      factor: 1.05,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Detecting DB sync state has reached minimum progress',
        this.logger
      )
    })
    this.logger.debug({ module: 'HasuraBackgroundClient' }, 'DB sync state has reached minimum progress')
    this.state = 'initialized'
    this.logger.info({ module: 'HasuraBackgroundClient' }, 'Initialized')
  }

  public async shutdown () {
    this.state = null
  }

  public async applySchemaAndMetadata (): Promise<void> {
    if (this.applyingSchemaAndMetadata) return
    this.applyingSchemaAndMetadata = true
    await pRetry(async () => {
      await this.hasuraCli('migrate --database-name default apply --down all')
      await this.hasuraCli('migrate --database-name default apply --up all')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor(
        'Applying PostgreSQL schema migrations',
        this.logger
      )
    })
    await pRetry(async () => {
      await this.hasuraCli('metadata clear')
      await this.hasuraCli('metadata apply')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor('Applying Hasura metadata', this.logger)
    })
    this.applyingSchemaAndMetadata = false
  }

  public async deleteAssetsAfterSlot (slotNo: Block['slotNo']): Promise<number> {
    this.logger.debug(
      { module: 'HasuraClient', slotNo },
      'deleting assets found in tokens after slot'
    )

    const result = await this.client.request(
      gql`mutation DeleteAssetsAfterSlot($slotNo: Int!) {
          delete_assets(
              where: {
                  firstAppearedInSlot: {
                      _gt: $slotNo
                  }
              }
          ) {
              affected_rows
          }
      }`,
      {
        slotNo
      }
    )
    return result.delete_assets.affected_rows
  }

  public async hasAsset (assetId: Asset['assetId']): Promise<boolean> {
    const result = await this.client.request(
      gql`query HasAsset (
          $assetId: bytea!
      ) {
          assets (
              where: { assetId: { _eq: $assetId }}
          ) {
              assetId
          }
      }`, {
        assetId: withHexPrefix(assetId)
      }
    )
    const response = result.assets.length > 0
    this.logger.debug(
      { module: 'HasuraClient', assetId, hasAsset: response },
      'Has asset?'
    )
    return response
  }

  public async getMostRecentPointWithNewAsset (): Promise<Schema.Point | null> {
    let point: Schema.Point | null
    // Handles possible race condition between the internal chain-follower, which manages the Asset table,
    // and cardano-db-sync's which managed the block table.
    await pRetry(async () => {
      // An offset of 1 is applied to ensure a partial block extraction is not skipped
      const result = await this.client.request(
        gql`query {
            assets (
                limit: 1
                offset: 1
                order_by: { firstAppearedInBlock: { slotNo: desc }}
            ) {
                firstAppearedInBlock {
                    hash
                    slotNo
                }
            }
        }`
      )
      if (result.errors !== undefined) {
        throw new Error(result.errors)
      }
      if (result.assets.length !== 0) {
        if (result.assets[0].firstAppearedInBlock === null) {
          throw new Error('cardano-db-sync is lagging behind the asset sync operation.')
        }
        const { hash, slotNo } = result.assets[0].firstAppearedInBlock
        point = {
          hash: hash.substring(2),
          slot: Number(slotNo)
        }
      } else {
        point = null
      }
    }, {
      factor: 1.5,
      retries: 1000,
      onFailedAttempt: util.onFailedAttemptFor(
        'Getting the most recent point with a new asset',
        this.logger
      )
    })
    return point
  }

  public async addAssetMetadata (asset: AssetMetadataAndHash) {
    this.logger.info(
      { module: 'HasuraClient', assetId: asset.assetId },
      'Adding metadata to asset'
    )
    const result = await this.client.request(
      gql`mutation AddAssetMetadata(
          $assetId: bytea!
          $decimals: Int
          $description: String
          $logo: String
          $metadataHash: bpchar!
          $name: String
          $ticker: String
          $url: String
      ) {
          update_assets(
              where: {
                  assetId: { _eq: $assetId }
              },
              _set: {
                  decimals: $decimals
                  description: $description
                  logo: $logo
                  metadataHash: $metadataHash
                  name: $name
                  ticker: $ticker
                  url: $url
              }
          ) {
              affected_rows
              returning {
                  assetId
              }
          }
      }`,
      {
        ...asset,
        ...{ assetId: withHexPrefix(asset.assetId) }
      }
    )
    if (result.errors !== undefined) {
      throw new Error(result.errors)
    }
  }

  public async insertAssets (assets: AssetWithoutTokens[]) {
    this.logger.debug(
      { module: 'HasuraClient', qty: assets.length },
      'inserting assets found in tokens'
    )
    const result = await this.client.request(
      gql`mutation InsertAssets($assets: [Asset_insert_input!]!) {
          insert_assets(objects: $assets) {
              returning {
                  name
                  policyId
                  description
                  assetName
                  assetId
              }
              affected_rows
          }
      }`,
      {
        assets: assets.map(asset => ({
          ...asset,
          ...{
            assetId: withHexPrefix(asset.assetId),
            assetName: withHexPrefix(asset.assetName),
            policyId: withHexPrefix(asset.policyId)
          }
        }))
      }
    )
    return result
  }

  public async getAssetMetadataHashesById (assetIds: Asset['assetId'][]): Promise<AssetMetadataHashAndId[]> {
    const result = await this.client.request(
      gql`query AssetMetadataHashes (
          $assetIds: [bytea!]!
      ){
          assets (
              where: {
                  assetId: { _in: $assetIds }
              }) {
              assetId
              metadataHash
          }
      }`,
      {
        assetIds: assetIds.map(id => withHexPrefix(id))
      }
    )
    return result.assets
  }
}
