import { ApolloClient, gql, InMemoryCache, NormalizedCacheObject } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import util, { DataFetcher } from '@cardano-graphql/util'
import { exec } from 'child_process'
import fetch from 'cross-fetch'
import { DocumentNode, GraphQLSchema, print } from 'graphql'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import pRetry from 'p-retry'
import path from 'path'
import {
  Asset,
  AssetBalance,
  AssetSupply, Int_Comparison_Exp as IntComparisonExp,
  PaymentAddressSummary,
  Token,
  TransactionOutput
} from './graphql_types'
import { dummyLogger, Logger } from 'ts-log'
import BigNumber from 'bignumber.js'
import { AssetMetadata } from './DataSyncController'

export class HasuraClient {
  private client: ApolloClient<NormalizedCacheObject>
  private applyingSchemaAndMetadata: boolean
  public adaCirculatingSupplyFetcher: DataFetcher<AssetSupply['circulating']>
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraUri: string,
    pollingInterval: number,
    private logger: Logger = dummyLogger
  ) {
    this.applyingSchemaAndMetadata = false
    this.adaCirculatingSupplyFetcher = new DataFetcher<AssetSupply['circulating']>(
      'AdaCirculatingSupply',
      this.getAdaCirculatingSupply.bind(this),
      pollingInterval,
      this.logger
    )
    this.client = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false
      }),
      defaultOptions: {
        query: {
          fetchPolicy: 'network-only'
        }
      },
      link: createHttpLink({
        uri: `${this.hasuraUri}/v1/graphql`,
        fetch,
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      })
    })
  }

  private async getAdaCirculatingSupply (): Promise<AssetSupply['circulating']> {
    const result = await this.client.query({
      query: gql`query {
          utxos_aggregate {
              aggregate {
                  sum {
                      value
                  }
              }
          }
      }`
    })
    return result.data.utxos_aggregate.aggregate.sum.value
  }

  private async hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `${this.hasuraCliPath} --skip-update-check --project ${path.resolve(__dirname, '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          this.logger.debug(stdout)
          resolve()
        }
      )
    })
  }

  public async initialize () {
    this.logger.info('Initializing Hasura', { module: 'HasuraClient' })
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
    this.logger.info('Hasura initialized', { module: 'HasuraClient' })
    await this.adaCirculatingSupplyFetcher.initialize()
  }

  public async shutdown () {
    await this.adaCirculatingSupplyFetcher.shutdown()
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
        this.logger.error(error)
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

  public async getPaymentAddressSummary (address: string, atBlock?: number): Promise<PaymentAddressSummary> {
    const result = await this.client.query({
      query: gql`query PaymentAddressSummary (
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
      variables: { address, atBlock }
    })
    const map = new Map<Asset['assetId'], AssetBalance>()
    for (const utxo of result.data.utxos as TransactionOutput[]) {
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
            assetId: 'ada',
            assetName: 'ada',
            name: 'ada',
            policyId: ''
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
      utxosCount: result.data.utxos_aggregate.aggregate.count
    }
  }

  public async getMeta (nodeTipBlockNumber: number) {
    const result = await this.client.query({
      query: gql`query {
          epochs (limit: 1, order_by: { number: desc }) {
              number
          }
          cardano {
              tip {
                  epoch {
                      number
                  }
                  number
                  forgedAt
              }
          }}`
    })
    const { tip } = result.data?.cardano[0]
    const lastEpoch = result.data?.epochs[0]
    return {
      // cardano-db-sync writes the epoch record at the end of each epoch during times of bulk sync
      // The initialization state can be determined by comparing the last epoch record against the
      // tip
      initialized: lastEpoch.number === tip.epoch?.number,
      syncPercentage: (tip.number / nodeTipBlockNumber) * 100
    }
  }

  public async getDistinctAssetsInTokens (): Promise<Asset[]> {
    const result = await this.client.query({
      query: gql`query {
          tokens (distinct_on: assetId) {
              assetId
              assetName
              policyId
          }
      }`
    })
    return result.data.tokens as Asset[]
  }

  public async getAssetIds (): Promise<Asset['assetId'][]> {
    const result = await this.client.query({
      query: gql`query {
          assets {
              assetId
          }
      }`
    })
    return result.data.assets.map((asset: Asset) => asset.assetId)
  }

  public async getAssetsIncMetadata (metadataFetchAttempts?: IntComparisonExp): Promise<Asset[]> {
    const result = await this.client.query({
      query: gql`query AssetsIncMetadata (
          $metadataFetchAttempts: Int_comparison_exp
      ){
          assets (
              where: {
                  metadataFetchAttempts: $metadataFetchAttempts
              }
          ) {
              assetId
              assetName
              description
              name
              policyId
          }
      }`,
      variables: {
        metadataFetchAttempts
      }
    })
    return result.data.assets
  }

  public async getAssetsWithoutFingerprint (): Promise<Pick<Asset, 'assetId' | 'assetName' | 'policyId'>[]> {
    const result = await this.client.query({
      query: gql`query {
          assets (where: { fingerprint: { _is_null: true }}) {
              assetId
              assetName
              policyId
          }
      }`
    })
    return result.data.assets.map((asset: Asset) => ({
      ...asset,
      policyId: util.scalars.Hash28Hex.serialize(asset.policyId)
    }))
  }

  public async getAssetsWithoutMetadata (metadataFetchAttempts: IntComparisonExp): Promise<Asset[]> {
    const result = await this.client.query({
      query: gql`query IdsOfAssetsWithoutMetadata (
          $metadataFetchAttempts: Int_comparison_exp!
      ){
          assets (
              where: { 
                  _and: [
                      { metadataFetchAttempts: $metadataFetchAttempts },
                      { metadataHash: { _is_null: true }}
                  ]
              }) {
              assetId
              metadataFetchAttempts
          }
      }`,
      variables: {
        metadataFetchAttempts
      }
    })
    return result.data.assets
  }

  public addAssetFingerprint (assetId: Asset['assetId'], fingerprint: Asset['fingerprint']) {
    this.logger.debug('adding fingerprint to asset', { module: 'HasuraClient', value: { assetId, fingerprint } })
    return this.client.mutate({
      mutation: gql`mutation AddAssetFingerprint(
          $assetId: String!
          $fingerprint: bpchar!
      ) {
          update_assets(
              where: {
                  assetId: { _eq: $assetId }
              },
              _set: {
                  fingerprint: $fingerprint,
              }
          ) {
              returning {
                  assetId
              }
          }
      }`,
      variables: {
        assetId,
        fingerprint
      }
    })
  }

  public addMetadata (metadata: AssetMetadata, metadataHash: string) {
    this.logger.debug('adding metadata to asset', { module: 'HasuraClient', value: { assetId: metadata.subject, metadataHash } })
    return this.client.mutate({
      mutation: gql`mutation AddAssetMetadata(
          $ticker: String
          $assetId: String!
          $description: String
          $logo: String
          $metadataHash: bpchar!
          $name: String
          $unit: jsonb
          $url: String
      ) {
          update_assets(
              where: {
                  assetId: { _eq: $assetId }
              },
              _set: {
                  ticker: $ticker,
                  description: $description,
                  logo: $logo,
                  metadataHash: $metadataHash,
                  name: $name,
                  unit: $unit
                  url: $url
              }
          ) {
              returning {
                  assetId
                  description
                  name
              }
          }
      }`,
      variables: {
        ticker: metadata.ticker?.value,
        assetId: metadata.subject,
        description: metadata.description?.value,
        logo: metadata.logo?.value,
        metadataHash,
        name: metadata.name?.value,
        unit: metadata.unit ? JSON.stringify(metadata.unit.value) : undefined,
        url: metadata.url?.value
      }
    })
  }

  public incrementMetadataFetchAttempts (assetId: Asset['assetId']) {
    this.logger.debug('incrementing asset metadata fetch attempt', { module: 'HasuraClient', value: assetId })
    return this.client.mutate({
      mutation: gql`mutation IncrementAssetMetadataFetchAttempt(
          $assetId: String!
      ) {
          update_assets(
              where: {
                  assetId: { _eq: $assetId }
              },
              _inc: {
                  metadataFetchAttempts: 1
              }
          ) {
              returning {
                  assetId
                  metadataFetchAttempts
              }
          }
      }`,
      variables: {
        assetId
      }
    })
  }

  public async insertAssets (assets: Asset[]) {
    this.logger.info('inserting assets', { module: 'HasuraClient', value: assets.length })
    const result = await this.client.mutate({
      mutation: gql`mutation InsertAssets($assets: [Asset_insert_input!]!) {
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
      variables: {
        assets
      }
    })
    return result.data
  }
}
