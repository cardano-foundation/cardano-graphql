import { Schema } from '@cardano-ogmios/client'
import { exec } from 'child_process'
import util, { DataFetcher, ModuleState } from '@cardano-graphql/util'
import fetch from 'cross-fetch'
import { DocumentNode, GraphQLSchema, print } from 'graphql'
import { GraphQLClient, gql } from 'graphql-request'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import pRetry from 'p-retry'
import path from 'path'
import {
  AdaPots,
  Asset,
  AssetBalance,
  AssetSupply,
  Block,
  PaymentAddressSummary,
  ShelleyProtocolParams,
  Token,
  TransactionOutput
} from './graphql_types'
import { dummyLogger, Logger } from 'ts-log'
import BigNumber from 'bignumber.js'
import {
  AssetMetadataAndHash,
  AssetMetadataHashAndId,
  AssetWithoutTokens
} from './typeAliases'

export type AdaPotsToCalculateSupply = { circulating: AssetSupply['circulating'], reserves: AdaPots['reserves']}

const epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.'

const withHexPrefix = (value: string) => `\\x${value !== undefined ? value : ''}`

export class HasuraClient {
  private client: GraphQLClient
  private applyingSchemaAndMetadata: boolean
  public adaPotsToCalculateSupplyFetcher: DataFetcher<AdaPotsToCalculateSupply>
  private state: ModuleState
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraUri: string,
    pollingInterval: number,
    readonly lastConfiguredMajorVersion: number, // Todo: Depreciate
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.applyingSchemaAndMetadata = false
    this.adaPotsToCalculateSupplyFetcher = new DataFetcher<AdaPotsToCalculateSupply>(
      'AdaPotsToCalculateSupply',
      () => {
        try {
          return this.getAdaPotsToCalculateSupply()
        } catch (error) {
          if (error.message !== epochInformationNotYetAvailable) {
            throw error
          }
          this.logger.trace({ err: error })
        }
      },
      pollingInterval,
      this.logger
    )
    this.client = new GraphQLClient(
      `${this.hasuraUri}/v1/graphql`,
      {
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      }
    )
  }

  private async getAdaPotsToCalculateSupply (): Promise<AdaPotsToCalculateSupply> {
    const result = await this.client.request(
      gql`query {
          epochs (limit: 1, order_by: { number: desc }) {
              adaPots {
                  reserves
              }
          }
          rewards_aggregate {
              aggregate {
                  sum {
                      amount
                  }
              }
          }
          utxos_aggregate {
              aggregate {
                  sum {
                      value
                  }
              }
          }
          withdrawals_aggregate {
              aggregate {
                  sum {
                      amount
                  }
              }
          }
      }`
    )
    const {
      epochs,
      rewards_aggregate: rewardsAggregate,
      utxos_aggregate: utxosAggregate,
      withdrawals_aggregate: withdrawalsAggregate
    } = result
    if (epochs.length === 0 || epochs[0]?.adaPots === null) {
      this.logger.debug({ module: 'HasuraClient' }, epochInformationNotYetAvailable)
      throw new Error(epochInformationNotYetAvailable)
    }
    const rewards = new BigNumber(rewardsAggregate.aggregate.sum.amount)
    const utxos = new BigNumber(utxosAggregate.aggregate.sum.value)
    const withdrawals = new BigNumber(withdrawalsAggregate.aggregate.sum.amount)
    const withdrawableRewards = rewards.minus(withdrawals)
    return {
      circulating: utxos.plus(withdrawableRewards).toString(),
      reserves: epochs[0]?.adaPots.reserves
    }
  }

  private async hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `${this.hasuraCliPath} --skip-update-check --project ${path.resolve(__dirname, '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          this.logger.debug({ module: 'HasuraClient' }, stdout)
          resolve()
        }
      )
    })
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'HasuraClient' }, 'Initializing')
    await this.applySchemaAndMetadata()
    await pRetry(async () => {
      this.schema = await this.buildHasuraSchema()
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor(
        'Fetching Hasura schema via introspection',
        this.logger
      )
    })
    this.logger.debug({ module: 'HasuraClient' }, 'graphql-engine setup')
    await pRetry(async () => {
      const result = await this.client.request(
        gql`query {
            epochs (limit: 1, order_by: { number: desc }) {
                number
            }
        }`
      )
      if (result.epochs.length === 0) {
        this.logger.debug({ module: 'HasuraClient' }, epochInformationNotYetAvailable)
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
    this.logger.debug({ module: 'HasuraClient' }, 'DB sync state has reached minimum progress')
    await this.adaPotsToCalculateSupplyFetcher.initialize()
    this.state = 'initialized'
    this.logger.info({ module: 'HasuraClient' }, 'Initialized')
  }

  public async shutdown () {
    await this.adaPotsToCalculateSupplyFetcher.shutdown()
  }

  public async applySchemaAndMetadata (): Promise<void> {
    if (this.applyingSchemaAndMetadata) return
    this.applyingSchemaAndMetadata = true
    await pRetry(async () => {
      await this.hasuraCli('migrate apply --down all')
      await this.hasuraCli('migrate apply --up all')
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

  public async buildHasuraSchema () {
    const executor = async ({ document, variables }: { document: DocumentNode, variables?: Object }) => {
      const query = print(document)
      try {
        const fetchResult = await fetch(`${this.hasuraUri}/v1/graphql`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'X-Hasura-Role': 'cardano-graphql'
          },
          body: JSON.stringify({ query, variables })
        })
        return fetchResult.json()
      } catch (error) {
        this.logger.error({ err: error })
        throw error
      }
    }
    const coreTypes = [
      'Block',
      'Cardano',
      'Epoch',
      'Block',
      'Transaction'
    ]
    const schema = wrapSchema({
      schema: await introspectSchema(executor),
      executor
    })
    for (const t of coreTypes) {
      const gqlType = schema.getType(t)
      if (!gqlType) {
        throw new Error(`Remote schema is missing ${t}`)
      }
    }
    return schema
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

  public async getCurrentProtocolVersion (): Promise<ShelleyProtocolParams['protocolVersion']> {
    const result = await this.client.request(
      gql`query {
          epochs (limit: 1, order_by: { number: desc }) {
              protocolParams {
                  protocolVersion
              }
          }
      }`
    )
    return result.epochs[0]?.protocolParams.protocolVersion
  }

  public async getMostRecentPointWithNewAsset (): Promise<Schema.Point | null> {
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
    if (result.assets.length === 0) return null
    const { hash, slotNo } = result.assets[0].firstAppearedInBlock
    return {
      hash: hash.substring(2),
      slot: slotNo
    }
  }

  public async getPaymentAddressSummary (address: string, atBlock?: number): Promise<PaymentAddressSummary> {
    const result = await this.client.request(
      gql`query PaymentAddressSummary (
          $address: String!
          $atBlock: Int
      ){
          utxos (
              where: {
                  _and: {
                      address: { _eq: $address },
                      transaction: { block: { number: { _lte: $atBlock }}}
                  }
              }
          ) {
              value
              tokens {
                  asset {
                      assetId
                      assetName
                      decimals
                      description
                      fingerprint
                      logo
                      metadataHash
                      name
                      ticker
                      tokenMints {
                          quantity
                          transaction {
                              hash
                          }
                      }
                      tokenMints_aggregate {
                          aggregate {
                              avg {
                                  quantity
                              }
                              count
                              max {
                                  quantity
                              }
                              min {
                                  quantity
                              }
                              sum {
                                  quantity
                              }
                          }
                      }
                      url
                      policyId
                  }
                  quantity
              }
          }
          utxos_aggregate (
              where: {
                  _and: {
                      address: { _eq: $address },
                      transaction: { block: { number: { _lte: $atBlock }}}
                  }
              }
          ) {
              aggregate {
                  count
              }
          }
      }`,
      {
        address,
        atBlock
      }
    )
    const map = new Map<Asset['assetId'], AssetBalance>()
    for (const utxo of result.utxos as TransactionOutput[]) {
      if (map.has('ada')) {
        const current = map.get('ada')
        map.set('ada', {
          ...current,
          ...{
            quantity: new BigNumber(current.quantity)
              .plus(new BigNumber(utxo.value))
              .toString()
          }
        })
      } else {
        map.set('ada', {
          asset: {
            assetId: '\\xada',
            assetName: '\\xada',
            name: 'ada',
            policyId: '\\xada',
            tokenMints: [],
            tokenMints_aggregate: {
              aggregate: {
                avg: {
                  quantity: 'na'
                },
                count: 'na',
                max: {
                  quantity: 'na'
                },
                min: {
                  quantity: 'na'
                },
                sum: {
                  quantity: 'na'
                }
              },
              nodes: []
            }
          },
          quantity: utxo.value
        })
      }
      for (const token of utxo.tokens as Token[]) {
        if (map.has(token.asset.assetId)) {
          const current = map.get(token.asset.assetId)
          map.set(token.asset.assetId, {
            ...current,
            ...{
              quantity: new BigNumber(current.quantity)
                .plus(new BigNumber(token.quantity))
                .toString()
            }
          })
        } else {
          map.set(token.asset.assetId, token as unknown as AssetBalance)
        }
      }
    }
    return {
      assetBalances: [...map.values()],
      utxosCount: result.utxos_aggregate.aggregate.count
    }
  }

  public async getMeta (nodeTipSlotNumber: number) {
    const result = await this.client.request(
      gql`query {
          epochs (limit: 1, order_by: { number: desc }) {
              number
          }
          cardano {
              tip {
                  epoch {
                      number
                  }
                  slotNo
                  forgedAt
              }
          }}`
    )
    const { tip } = result?.cardano[0]
    const lastEpoch = result?.epochs[0]
    const syncPercentage = tip.slotNo / nodeTipSlotNumber * 100
    return {
      // cardano-db-sync writes the epoch record at the end of each epoch during times of bulk sync
      // The initialization state can be determined by comparing the last epoch record against the
      // tip
      initialized: lastEpoch.number === tip.epoch?.number,
      // we cannot assume that actual db-sync syncPercentage will be less or equal to node sync state due to race condition at the query time
      syncPercentage: syncPercentage > 100 ? 100 : syncPercentage
    }
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
}
