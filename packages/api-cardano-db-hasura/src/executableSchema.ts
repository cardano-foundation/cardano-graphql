import fs from 'fs'
import { ApolloError } from 'apollo-server'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { delegateToSchema } from '@graphql-tools/delegate'
import path from 'path'
import util from '@cardano-graphql/util'
import { Resolvers, Genesis } from './graphql_types'
import { HasuraClient } from './HasuraClient'
import {
  IPv4Resolver,
  IPv6Resolver,
  JSONResolver,
  JSONObjectResolver,
  TimestampResolver
} from 'graphql-scalars'
import { CardanoNodeClient } from './CardanoNodeClient'
import BigNumber from 'bignumber.js'
import {
  FieldsComplexityMapping,
  ComplexityMapping,
  defaultComplexity,
  getDefaultQueryComplexity
} from './queryComplexity'
const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  AssetFingerprint: util.scalars.AssetFingerprint,
  BigInt: GraphQLBigInt,
  DateTime: util.scalars.DateTimeUtcToIso,
  Hash28Hex: util.scalars.Hash28Hex,
  Hash32Hex: util.scalars.Hash32Hex,
  Hex: util.scalars.Hex,
  IPv4: IPv4Resolver,
  IPv6: IPv6Resolver,
  JSON: JSONResolver,
  JSONObject: JSONObjectResolver,
  Lovelace: util.scalars.Lovelace,
  Percentage: util.scalars.Percentage,
  StakeAddress: util.scalars.StakeAddress,
  StakePoolID: util.scalars.StakePoolID,
  Timestamp: TimestampResolver,
  VRFVerificationKey: util.scalars.VRFVerificationKey
} as any

export async function buildSchema (
  hasuraClient: HasuraClient,
  genesis: Genesis,
  cardanoNodeClient: CardanoNodeClient,
  customFieldsComplexity: FieldsComplexityMapping = defaultComplexity
) {
  const getComplexityExtension = (operation: string, queryName: string) => {
    if (operation in customFieldsComplexity) {
      const operationMapping = customFieldsComplexity[
        operation
      ] as ComplexityMapping
      if (
        queryName in operationMapping &&
        operationMapping[queryName].extensions
      ) {
        // If it has a custom complexity then use that one and ignore the base cost,
        // otherwise use the default with the base cost
        return {
          complexity:
            operationMapping[queryName].extensions.complexity ||
            getDefaultQueryComplexity(
              operationMapping[queryName].extensions.baseCost
            )
        }
      }
    }
    // If not found, then just return the default complexity estimators
    return { complexity: getDefaultQueryComplexity() }
  }
  return makeExecutableSchema({
    resolvers: Object.assign({}, scalarResolvers, customFieldsComplexity, {
      Mutation: {
        submitTransaction: {
          resolve: async (_root: any, args: { transaction: string }) => {
            try {
              const hash = await cardanoNodeClient.submitTransaction(
                args.transaction
              )
              return { hash }
            } catch (error) {
              if (Array.isArray(error)) {
                throw new ApolloError('Invalid Transaction', 'BAD_REQUEST', {
                  reasons: error.map(e => ({
                    name: e.name,
                    details: JSON.parse(e.message)
                  }))
                })
              } else {
                throw new ApolloError(error)
              }
            }
          },
          selectionSet: null,
          extensions: getComplexityExtension('Mutation', 'submitTransaction')
        }
      },
      PaymentAddress: {
        summary: {
          resolve: async (parent: { address: string }, args: { atBlock: number }) => {
            try {
              return await hasuraClient.getPaymentAddressSummary(
                parent.address,
                args.atBlock
              )
            } catch (error) {
              throw new ApolloError(error)
            }
          },
          selectionSet: null,
          extensions: getComplexityExtension('PaymentAddress', 'summary')
        }
      },
      Query: {
        activeStake: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'activeStake',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'activeStake')
        },
        activeStake_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'activeStake_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'activeStake_aggregate')
        },
        ada: {
          resolve: async () => {
            const adaPots = hasuraClient.adaPotsToCalculateSupplyFetcher.value
            if (adaPots === undefined || adaPots === null) {
              return new ApolloError(
                'ada query results are not ready yet. This can occur during startup.'
              )
            }
            return {
              supply: {
                circulating: adaPots.circulating,
                max: genesis.shelley.maxLovelaceSupply,
                total: new BigNumber(genesis.shelley.maxLovelaceSupply)
                  .minus(new BigNumber(adaPots.reserves))
                  .toString()
              }
            }
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'ada')
        },
        assets: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'assets',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'assets')
        },
        assets_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'assets_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'assets_aggregate')
        },
        blocks: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'blocks',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'blocks')
        },
        blocks_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'blocks_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'blocks_aggregate')
        },
        cardano: {
          resolve: async (_root: any, _args: any, context: any, info: any) => {
            try {
              const result = await delegateToSchema({
                context,
                fieldName: 'cardano',
                info,
                operation: 'query',
                schema: hasuraClient.schema
              })
              if (result[0]?.currentEpoch === null) {
                return new ApolloError(
                  'currentEpoch is only available when close to the chain tip. This is expected during the initial chain-sync.'
                )
              }
              return result[0]
            } catch (error) {
              throw new ApolloError(error)
            }
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'cardano')
        },
        cardanoDbMeta: {
          resolve: async () => {
            try {
              const slotNo = await cardanoNodeClient.getTipSlotNo()
              return hasuraClient.getMeta(slotNo)
            } catch (error) {
              throw new ApolloError(
                error.name === 'ModuleIsNotInitialized'
                  ? 'cardanoDbMeta query is not ready. Try again shortly'
                  : error.message
              )
            }
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'cardanoDbMeta')
        },
        collateralInputs: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'collateralInputs',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'collateralInputs')
        },
        collateralInputs_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'collateralInputs_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'collateralInputs_aggregate')
        },
        collateralOutputs: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'collateralOutputs',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'collateralOutputs')
        },
        collateralOutputs_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'collateralOutputs_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'collateralOutputs_aggregate')
        },
        committee: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'committee',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'committee')
        },
        committee_registration: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'committee_registration',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'committee_registration')
        },
        committee_de_registration: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'committee_de_registration',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'committee_de_registration')
        },
        delegations: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'delegations',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'delegations')
        },
        delegationvotes: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'delegationvotes',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'delegationvotes')
        },
        delegations_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'delegations_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'delegations_aggregate')
        },
        drepRegistrations: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'drepRegistrations',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'drepRegistrations')
        },
        epochs: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'epochs',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'epochs')
        },
        epochs_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'epochs_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'epochs_aggregate')
        },
        genesis: {
          resolve: async () => genesis,
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'genesis')
        },
        paymentAddresses: {
          resolve: async (_root: any, args: { addresses: any[] }) => {
            return args.addresses.map(async (address) => {
              return { address }
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'paymentAddresses')
        },
        redeemers: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'redeemers',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'redeemers')
        },
        redeemers_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'redeemers_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'redeemers_aggregate')
        },
        rewards: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'rewards',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'rewards')
        },
        rewards_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'rewards_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'rewards_aggregate')
        },
        scripts: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'scripts',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'scripts')
        },
        scripts_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'scripts_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'scripts_aggregate')
        },
        stakeDeregistrations: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakeDeregistrations',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'stakeDeregistrations')
        },
        stakeDeregistrations_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakeDeregistrations_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension(
            'Query',
            'stakeDeregistrations_aggregate'
          )
        },
        stakePools: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakePools',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'stakePools')
        },
        stakePools_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakePools_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'stakePools_aggregate')
        },
        stakeRegistrations: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakeRegistrations',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'stakeRegistrations')
        },
        stakeRegistrations_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'stakeRegistrations_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension(
            'Query',
            'stakeRegistrations_aggregate'
          )
        },
        transactions: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'transactions',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'transactions')
        },
        transactions_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'transactions_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'transactions_aggregate')
        },
        tokenMints: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'tokenMints',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'tokenMints')
        },
        tokenMints_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'tokenMints_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'tokenMints_aggregate')
        },
        treasury_withdrawal: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'treasury_withdrawal',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'treasury_withdrawal')
        },
        utxos: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'utxos',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'utxos')
        },
        utxos_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'utxos_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'utxos_aggregate')
        },
        voting_anchor: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'voting_anchor',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'voting_anchor')
        },
        voting_procedure: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'voting_procedure',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'voting_procedure')
        },
        withdrawals: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'withdrawals',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'withdrawals')
        },
        withdrawals_aggregate: {
          resolve: (_root: any, args: any, context: any, info: any) => {
            return delegateToSchema({
              args,
              context,
              fieldName: 'withdrawals_aggregate',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
          },
          selectionSet: null,
          extensions: getComplexityExtension('Query', 'withdrawals_aggregate')
        }
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(
      path.resolve(__dirname, '..', 'schema.graphql'),
      'utf-8'
    )
  })
}
