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
  TimestampResolver,
  URLResolver
} from 'graphql-scalars'
import { CardanoNodeClient } from './CardanoNodeClient'
const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  AssetFingerprint: util.scalars.AssetFingerprint,
  BigInt: GraphQLBigInt,
  DateTime: util.scalars.DateTimeUtcToIso,
  Hash28Hex: util.scalars.Hash28Hex,
  Hash32Hex: util.scalars.Hash32Hex,
  IPv4: IPv4Resolver,
  IPv6: IPv6Resolver,
  JSON: JSONResolver,
  JSONObject: JSONObjectResolver,
  Percentage: util.scalars.Percentage,
  StakeAddress: util.scalars.StakeAddress,
  StakePoolID: util.scalars.StakePoolID,
  Timestamp: TimestampResolver,
  URL: URLResolver,
  VRFVerificationKey: util.scalars.VRFVerificationKey
} as any

export async function buildSchema (
  hasuraClient: HasuraClient,
  genesis: Genesis,
  cardanoNodeClient: CardanoNodeClient
) {
  const throwIfNotInCurrentEra = async (queryName: string) => {
    if (!(await cardanoNodeClient.isInCurrentEra())) {
      return new ApolloError(`${queryName} results are only available when close to the network tip. This is expected during the initial chain-sync.`)
    }
  }
  return makeExecutableSchema({
    resolvers: Object.assign({}, scalarResolvers, {
      Mutation: {
        submitTransaction: async (_root, args) => {
          await throwIfNotInCurrentEra('submitTransaction')
          const hash = await cardanoNodeClient.submitTransaction(args.transaction)
          return { hash }
        }
      },
      PaymentAddress: {
        summary: async (parent, args) => {
          try {
            return await hasuraClient.getPaymentAddressSummary(parent.address, args.atBlock)
          } catch (error) {
            throw new ApolloError(error)
          }
        }
      },
      Query: {
        activeStake: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'activeStake',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        activeStake_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'activeStake_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        ada: async () => {
          await throwIfNotInCurrentEra('ada')
          return {
            supply: {
              circulating: hasuraClient.adaCirculatingSupplyFetcher.value,
              max: genesis.shelley.maxLovelaceSupply
            }
          }
        },
        assets: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'assets',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        assets_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'assets_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        blocks: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'blocks',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        blocks_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'blocks_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        cardano: async (_root, _args, context, info) => {
          try {
            const result = await delegateToSchema({
              context,
              fieldName: 'cardano',
              info,
              operation: 'query',
              schema: hasuraClient.schema
            })
            if (result[0].currentEpoch === null) {
              return new ApolloError('currentEpoch is only available when close to the chain tip. This is expected during the initial chain-sync.')
            }
            return result[0]
          } catch (error) {
            throw new ApolloError(error)
          }
        },
        cardanoDbMeta: async () => {
          try {
            const tip = await cardanoNodeClient.getTip()
            return hasuraClient.getMeta(tip.blockNo)
          } catch (error) {
            throw new ApolloError(error)
          }
        },
        delegations: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'delegations',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        delegations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'delegations_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        epochs: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'epochs',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        epochs_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'epochs_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        genesis: async () => genesis,
        paymentAddresses: async (_root, args) => {
          await throwIfNotInCurrentEra('addressSummary')
          return args.addresses.map(async (address) => {
            return { address }
          })
        },
        rewards: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'rewards',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        rewards_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'rewards_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakeDeregistrations: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeDeregistrations',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakeDeregistrations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeDeregistrations_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakePools: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakePools',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakePools_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakePools_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakeRegistrations: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeRegistrations',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        stakeRegistrations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeRegistrations_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        transactions: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'transactions',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        transactions_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'transactions_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        utxos: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'utxos',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        utxos_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'utxos_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        withdrawals: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'withdrawals',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        },
        withdrawals_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'withdrawals_aggregate',
            info,
            operation: 'query',
            schema: hasuraClient.schema
          })
        }
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
