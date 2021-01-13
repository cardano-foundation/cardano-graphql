import { ApolloClient, gql, InMemoryCache, NormalizedCacheObject } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import util, { DataFetcher } from '@cardano-graphql/util'
import { exec } from 'child_process'
import fetch from 'cross-fetch'
import dayjs from 'dayjs'
import utc from 'dayjs/plugin/utc'
import { DocumentNode, GraphQLSchema, print } from 'graphql'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import pRetry from 'p-retry'
import path from 'path'
import {
  AssetBalance,
  AssetSupply,
  PaymentAddressSummary,
  Token,
  TransactionOutput
} from './graphql_types'
import { dummyLogger, Logger } from 'ts-log'
import BigNumber from 'bignumber.js'

dayjs.extend(utc)

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

  public async initialize () {
    this.logger.info('Initializing Hasura', { module: 'HasuraClient' })
    await this.applySchemaAndMetadata()
    await pRetry(async () => {
      this.schema = await this.buildHasuraSchema()
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor('Fetching Hasura schema via introspection')
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
      onFailedAttempt: util.onFailedAttemptFor('Applying PostgreSQL schema migrations')
    })
    await pRetry(async () => {
      await this.hasuraCli('metadata clear')
      await this.hasuraCli('metadata apply')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor('Applying Hasura metadata')
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
                  assetId
                  assetName
                  policyId
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
    const map = new Map<Token['assetId'], AssetBalance>()
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
          assetId: 'ada',
          assetName: 'ada',
          policyId: '',
          quantity: utxo.value
        })
      }
      for (const token of utxo.tokens as Token[]) {
        if (map.has(token.assetId)) {
          const current = map.get(token.assetId)
          map.set(token.assetId, {
            ...current,
            ...{
              quantity: new BigNumber(current.quantity)
                .plus(new BigNumber(token.quantity))
                .toString()
            }
          })
        } else {
          map.set(token.assetId, token as unknown as AssetBalance)
        }
      }
    }
    return {
      assetBalances: [...map.values()],
      utxosCount: result.data.utxos_aggregate.aggregate.count
    }
  }

  public async getMeta () {
    const result = await this.client.query({
      query: gql`query {
          cardano {
              tip {
                  forgedAt
              }
          }}`
    })
    const { tip } = result.data?.cardano[0]
    const currentUtc = dayjs().utc()
    const tipUtc = dayjs.utc(tip.forgedAt)
    return {
      initialized: tipUtc.isAfter(currentUtc.subtract(120, 'second')),
      syncPercentage: (tipUtc.valueOf() / currentUtc.valueOf()) * 100
    }
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
}
