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
  AdaPots,
  Asset,
  AssetBalance,
  AssetSupply,
  Int_Comparison_Exp as IntComparisonExp,
  PaymentAddressSummary,
  ShelleyProtocolParams,
  Token,
  TransactionOutput
} from './graphql_types'
import { dummyLogger, Logger } from 'ts-log'
import BigNumber from 'bignumber.js'
import { AssetWithoutTokens } from './typeAliases'

export type AdaPotsToCalculateSupply = { circulating: AssetSupply['circulating'], reserves: AdaPots['reserves']}

export class HasuraClient {
  private client: ApolloClient<NormalizedCacheObject>
  private applyingSchemaAndMetadata: boolean
  public adaPotsToCalculateSupplyFetcher: DataFetcher<AdaPotsToCalculateSupply>
  public currentProtocolVersionFetcher: DataFetcher<ShelleyProtocolParams['protocolVersion']>
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraUri: string,
    pollingInterval: number,
    readonly lastConfiguredMajorVersion: number,
    private logger: Logger = dummyLogger
  ) {
    this.applyingSchemaAndMetadata = false
    this.adaPotsToCalculateSupplyFetcher = new DataFetcher<AdaPotsToCalculateSupply>(
      'AdaPotsToCalculateSupply',
      () => {
        try {
          return this.getAdaPotsToCalculateSupply()
        } catch (error) {
          if (error.message !== 'currentEpoch is only available when close to the chain tip. This is expected during the initial chain-sync.') {
            throw error
          }
          this.logger.debug(error.message)
        }
      },
      pollingInterval,
      this.logger
    )
    this.currentProtocolVersionFetcher = new DataFetcher<ShelleyProtocolParams['protocolVersion']>(
      'ProtocolParams',
      async () => {
        const getCurrentProtocolVersion = await this.getCurrentProtocolVersion()
        this.logger.debug(getCurrentProtocolVersion)
        return getCurrentProtocolVersion
      },
      1000 * 60,
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

  private async getAdaPotsToCalculateSupply (): Promise<AdaPotsToCalculateSupply> {
    const result = await this.client.query({
      query: gql`query {
          cardano { 
              currentEpoch {
                  adaPots {
                      reserves
                  }
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
    })
    const {
      cardano,
      rewards_aggregate: rewardsAggregate,
      utxos_aggregate: utxosAggregate,
      withdrawals_aggregate: withdrawalsAggregate
    } = result.data
    const rewards = new BigNumber(rewardsAggregate.aggregate.sum.amount)
    const utxos = new BigNumber(utxosAggregate.aggregate.sum.value)
    const withdrawals = new BigNumber(withdrawalsAggregate.aggregate.sum.amount)
    const withdrawableRewards = rewards.minus(withdrawals)
    return {
      circulating: utxos.plus(withdrawableRewards).toString(),
      reserves: cardano[0].currentEpoch.adaPots.reserves
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
          this.logger.debug(stdout)
          resolve()
        }
      )
    })
  }

  public async initialize () {
    this.logger.info('Initializing', { module: 'HasuraClient' })
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
    this.logger.debug('graphql-engine setup', { module: 'HasuraClient' })
    await pRetry(async () => {
      const result = await this.client.query({
        query: gql`query {
            cardano {
                currentEpoch {
                    number
                }
            }
        }`
      })
      if (result.data.cardano.currentEpoch === null) {
        this.logger.debug('Not in current era')
        throw new Error('Not in current era')
      }
    }, {
      factor: 1.05,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Detecting sync state being in current era',
        this.logger
      )
    })
    this.logger.debug('DB is in current era', { module: 'HasuraClient' })
    await this.currentProtocolVersionFetcher.initialize()
    await this.adaPotsToCalculateSupplyFetcher.initialize()
    this.logger.info('Initialized', { module: 'HasuraClient' })
  }

  public async shutdown () {
    await this.adaPotsToCalculateSupplyFetcher.shutdown()
    await this.currentProtocolVersionFetcher.shutdown()
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

  public async getCurrentProtocolVersion (): Promise<ShelleyProtocolParams['protocolVersion']> {
    const result = await this.client.query({
      query: gql`query {
          epochs (limit: 1, order_by: { number: desc }) {
              protocolParams {
                  protocolVersion
              }
          }
      }`
    })
    return result.data?.epochs[0].protocolParams.protocolVersion
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
            policyId: '',
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

  public async getDistinctAssetsInTokens (options?: { limit: number, offset: number }): Promise<AssetWithoutTokens[]> {
    const result = await this.client.query({
      query: gql`query DistinctAssetsInTokens (
          $limit: Int
          $offset: Int
      ) {
          tokenMints (
              distinct_on: assetId
              limit: $limit
              order_by: { assetId: asc }
              offset: $offset
          ) {
              assetId
              assetName
              policyId
          }
      }`,
      variables: {
        limit: options?.limit,
        offset: options?.offset
      }
    })
    return result.data.tokenMints as AssetWithoutTokens[]
  }

  public async distinctAssetsInTokensCount (): Promise<number> {
    const result = await this.client.query({
      query: gql`query {
          tokenMints_aggregate (distinct_on: assetId) {
              aggregate {
                  count
              }
          }
      }`
    })
    return result.data.tokenMints_aggregate.aggregate.count
  }

  public async assetsEligibleForMetadataRefreshCount (metadataFetchAttempts: IntComparisonExp): Promise<number> {
    try {
      const result = await this.client.query({
        query: gql`query AssetsEligibleForMetadataRefreshCount (
            $metadataFetchAttempts: Int_comparison_exp!
        ) {
            assets_aggregate (
                where: {
                    metadataFetchAttempts: $metadataFetchAttempts
                }) {
                aggregate {
                    count
                }
            }
        }`,
        variables: {
          metadataFetchAttempts
        }
      })
      return result.data.assets_aggregate.aggregate.count
    } catch (error) {
      this.logger.error(error)
      throw error
    }
  }

  public async getAssetsIncMetadata (metadataFetchAttempts: IntComparisonExp, options: { limit: number, offset: number }): Promise<AssetWithoutTokens[]> {
    const result = await this.client.query({
      query: gql`query AssetsIncMetadata (
          $metadataFetchAttempts: Int_comparison_exp
          $limit: Int
          $offset: Int
      ){
          assets (
              limit: $limit
              offset: $offset
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
        metadataFetchAttempts,
        limit: options.limit,
        offset: options.offset
      }
    })
    return result.data.assets
  }

  public async hasAssetsWithoutFingerprint (): Promise<boolean> {
    const result = await this.client.query({
      query: gql`query {
          assets_aggregate (
              where: { fingerprint: { _is_null: true }}
          ) {
              aggregate {
                  count  
              }
          }
      }`
    })
    this.logger.debug(
      'Assets without a fingerprint stored',
      { module: 'HasuraClient', value: result.data.assets_aggregate.aggregate.count }
    )
    return new BigNumber(result.data.assets_aggregate.aggregate.count).isGreaterThan(0)
  }

  public async getAssetsById (assetIds: Asset['assetId'][]): Promise<AssetWithoutTokens[]> {
    const result = await this.client.query({
      query: gql`query IdsOfAssetsWithoutMetadata (
          $assetIds: [String!]!
      ){
          assets (
              where: {
                  assetId: { _in: $assetIds }
              }) {
              assetId
          }
      }`,
      variables: {
        assetIds
      }
    })
    return result.data.assets
  }

  public async getAssetsWithoutFingerprint (limit?: number): Promise<Pick<AssetWithoutTokens, 'assetId' | 'assetName' | 'policyId'>[]> {
    const result = await this.client.query({
      query: gql`query AssetsWithoutFingerprint (
        $limit: Int
      ) {
          assets (
              limit: $limit,
              order_by: { assetId: asc }
              where: { fingerprint: { _is_null: true }}
          ) {
              assetId
              assetName
              policyId
          }
      }`,
      variables: {
        limit
      }
    })
    return result.data.assets.map((asset: AssetWithoutTokens) => ({
      ...asset,
      policyId: util.scalars.Hash28Hex.serialize(asset.policyId)
    }))
  }

  public async assetsWithoutMetadataCount (metadataFetchAttempts: IntComparisonExp): Promise<number> {
    try {
      const result = await this.client.query({
        query: gql`query AssetsWithoutMetadataCount (
            $metadataFetchAttempts: Int_comparison_exp!
        ) {
            assets_aggregate (
                where: {
                    _and: [
                        { metadataFetchAttempts: $metadataFetchAttempts },
                        { metadataHash: { _is_null: true }}
                    ]
                }) {
                aggregate {
                    count
                }
            }
        }`,
        variables: {
          metadataFetchAttempts
        }
      })
      return result.data.assets_aggregate.aggregate.count
    } catch (error) {
      this.logger.error(error)
      throw error
    }
  }

  public async getAssetsWithoutMetadata (
    metadataFetchAttempts: IntComparisonExp,
    options?: { limit: number, offset: number }
  ): Promise<AssetWithoutTokens[]> {
    const result = await this.client.query({
      query: gql`query IdsOfAssetsWithoutMetadata (
          $limit: Int
          $metadataFetchAttempts: Int_comparison_exp!
          $offset: Int
      ){
          assets (
              limit: $limit
              order_by: { assetId: asc }
              offset: $offset
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
        metadataFetchAttempts,
        limit: options?.limit,
        offset: options?.offset
      }
    })
    return result.data.assets
  }

  public async isInCurrentEra () {
    const protocolVersion = this.currentProtocolVersionFetcher.value
    this.logger.debug('Comparing current protocol params with last known major version from cardano-node config', {
      module: 'CardanoNodeClient',
      value: {
        currentProtocolVersion: protocolVersion,
        lastConfiguredMajorVersion: protocolVersion.major
      }
    })
    return protocolVersion.major >= this.lastConfiguredMajorVersion
  }

  public addAssetFingerprints (assets: Pick<AssetWithoutTokens, 'assetId' | 'fingerprint'>[]) {
    this.logger.debug('Adding fingerprint to assets', { module: 'HasuraClient', value: assets.length })
    return this.client.mutate({
      mutation: gql`mutation AddAssetFingerprint($assets: [Asset_insert_input!]!) {
          insert_assets(
              objects: $assets,
              on_conflict: {
                  constraint: Asset_pkey,
                  update_columns: [fingerprint]
              }
          ) {
              returning {
                  assetId
              }
          }
      }`,
      variables: {
        assets
      }
    })
  }

  public addMetadata (assets: (Pick<AssetWithoutTokens, 'assetId' | 'description' | 'logo' | 'name' | 'ticker' | 'url'> & { metadataHash: string })[]) {
    this.logger.info('Adding metadata to assets', { module: 'HasuraClient', value: assets.length })
    return this.client.mutate({
      mutation: gql`mutation AddAssetMetadata($assets: [Asset_insert_input!]!) {
          insert_assets(
              objects: $assets,
              on_conflict: {
                  constraint: Asset_pkey,
                  update_columns: [
                      description,
                      logo,
                      metadataHash,
                      name,
                      ticker,
                      url
                  ]
              }
          ) {
              returning {
                  assetId
              }
          }
      }`,
      variables: {
        assets
      }
    })
  }

  public incrementMetadataFetchAttempts (assetIds: AssetWithoutTokens['assetId'][]) {
    this.logger.debug(
      'Incrementing metadata fetch attempt',
      { module: 'HasuraClient', value: assetIds.length }
    )
    return this.client.mutate({
      mutation: gql`mutation IncrementAssetMetadataFetchAttempt(
          $assetIds: [String!]!
      ) {
          update_assets(
              where: {
                  assetId: { _in: $assetIds }
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
        assetIds
      }
    })
  }

  public async insertAssets (assets: AssetWithoutTokens[]) {
    this.logger.info('inserting assets found in tokens', { module: 'HasuraClient', value: assets.length })
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
