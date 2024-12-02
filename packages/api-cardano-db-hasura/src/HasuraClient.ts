import util, { DataFetcher, ModuleState } from '@cardano-graphql/util'
import fetch from 'cross-fetch'
import { DocumentNode, GraphQLSchema, print } from 'graphql'
import { GraphQLClient, gql } from 'graphql-request'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import pRetry from 'p-retry'
import {
  AdaPots,
  Asset,
  AssetBalance,
  AssetSupply,
  PaymentAddressSummary,
  ProtocolParams,
  Token,
  TransactionOutput
} from './graphql_types'
import { dummyLogger, Logger } from 'ts-log'
import BigNumber from 'bignumber.js'

export type AdaPotsToCalculateSupply = { circulating: AssetSupply['circulating'], reserves: AdaPots['reserves']}

const epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.'

export class HasuraClient {
  private client: GraphQLClient
  public adaPotsToCalculateSupplyFetcher: DataFetcher<AdaPotsToCalculateSupply>
  private state: ModuleState
  public schema: GraphQLSchema

  constructor (
    readonly hasuraUri: string,
    pollingInterval: number,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.adaPotsToCalculateSupplyFetcher = new DataFetcher<AdaPotsToCalculateSupply>(
      'AdaPotsToCalculateSupply',
      () => {
        try {
          return this.getAdaPotsToCalculateSupply()
        } catch (error) {
          if (error.message !== epochInformationNotYetAvailable) {
            console.debug(error.message)
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

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'HasuraClient' }, 'Initializing')
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
    // await pRetry(async () => {
    //   await this.adaPotsToCalculateSupplyFetcher.initialize()
    // }, {
    //   factor: 1.1,
    //   forever: true,
    //   maxTimeout: 15000,
    //   onFailedAttempt: util.onFailedAttemptFor(
    //     'Initializing data fetchers',
    //     this.logger
    //   )
    // })
    this.logger.debug({ module: 'HasuraClient' }, 'Data fetchers initialized')
    this.state = 'initialized'
    this.logger.info({ module: 'HasuraClient' }, 'Initialized')
  }

  public async shutdown () {
    await this.adaPotsToCalculateSupplyFetcher.shutdown()
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

  public async getCurrentProtocolVersion (): Promise<ProtocolParams['protocolVersion']> {
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

  public async getPaymentAddressSummary (address: string, atBlock?: number): Promise<PaymentAddressSummary> {
    let args = 'address: { _eq: $address }'
    if (atBlock) {
      args = args + '\n transaction: { block: { number: { _lte: $atBlock }}}'
    }
    const query = `query PaymentAddressSummary (
          $address: String!
          $atBlock: Int
      ){
          utxos (
              where: {
                  _and: {
                      ${args}
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
                      ${args}
                  }
              }
          ) {
              aggregate {
                  count
              }
          }
      }`
    const result = await this.client.request(
      gql`${query}`,
      { address, atBlock }
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
}
