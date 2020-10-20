import fs from 'fs'
import { ApolloError } from 'apollo-server'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { delegateToSchema } from '@graphql-tools/delegate'
import pRetry from 'p-retry'
import path from 'path'
import util from '@cardano-graphql/util'
import { Resolvers } from './graphql_types'
import { HasuraClient } from './HasuraClient'
import { GraphQLSchema } from 'graphql'

const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  Hash28Hex: util.scalars.Hash28Hex,
  Hash32Hex: util.scalars.Hash32Hex,
  BigInt: GraphQLBigInt,
  DateTime: util.scalars.DateTimeUtcToIso,
  Percentage: util.scalars.Percentage
} as any

export async function buildSchema (hasuraClient: HasuraClient) {
  let hasuraSchema: GraphQLSchema
  await pRetry(async () => {
    hasuraSchema = await hasuraClient.buildHasuraSchema()
  }, {
    factor: 1.75,
    retries: 9,
    onFailedAttempt: util.onFailedAttemptFor('Fetching Hasura schema via introspection')
  })
  return makeExecutableSchema({
    resolvers: Object.assign({}, scalarResolvers, {
      Query: {
        activeStake: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'activeStake',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        blocks: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'blocks',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        blocks_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'blocks_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        cardano: async (_root, _args, context, info) => {
          try {
            const result = await delegateToSchema({
              context,
              fieldName: 'cardano',
              info,
              operation: 'query',
              schema: hasuraSchema
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
            return hasuraClient.getMeta()
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
            schema: hasuraSchema
          })
        },
        delegations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'delegations_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        epochs: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'epochs',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        epochs_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'epochs_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        rewards: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'rewards',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        rewards_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'rewards_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakeDeregistrations: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeDeregistrations',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakeDeregistrations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeDeregistrations_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakePools: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakePools',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakePools_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakePools_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakeRegistrations: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeRegistrations',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        stakeRegistrations_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'stakeRegistrations_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        transactions: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'transactions',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        transactions_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'transactions_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        utxos: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'utxos',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        utxos_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'utxos_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        withdrawals: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'withdrawals',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        },
        withdrawals_aggregate: (_root, args, context, info) => {
          return delegateToSchema({
            args,
            context,
            fieldName: 'withdrawals_aggregate',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
        }
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
