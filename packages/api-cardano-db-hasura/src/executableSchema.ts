import fs from 'fs'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { delegateToSchema } from '@graphql-tools/delegate'
import { Resolvers } from './graphql_types'
import path from 'path'
import { scalarResolvers } from './lib/scalar_resolvers'
import { buildHasuraSchema } from './buildHasuraSchema'
import pRetry from 'p-retry'
import util from '@cardano-graphql/util'
import { GraphQLSchema } from 'graphql'

export async function buildSchema (hasuraUri: string) {
  let hasuraSchema: GraphQLSchema
  await pRetry(async () => {
    hasuraSchema = await buildHasuraSchema(hasuraUri)
  }, {
    retries: 9,
    onFailedAttempt: util.onFailedAttemptFor('Hasura schema introspection')
  })
  return makeExecutableSchema({
    resolvers: Object.assign({}, scalarResolvers, {
      Query: {
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
        cardano: async (_root, _args, context, info) => {
          const result = await delegateToSchema({
            context,
            fieldName: 'cardano',
            info,
            operation: 'query',
            schema: hasuraSchema
          })
          return result[0]
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
        }
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
