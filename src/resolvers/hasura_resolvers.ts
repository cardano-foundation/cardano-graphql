import { delegateToSchema } from 'graphql-tools'
import { Resolvers } from '../graphql_types'

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'blocks',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    blocks_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'blocks_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'epochs',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'epochs_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    cardano: async (_root, _args, context, info) => {
      const result = await delegateToSchema({
        context,
        fieldName: 'cardano',
        info,
        operation: 'query',
        schema: context.hasura
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
        schema: context.hasura
      })
    },
    transactions_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'transactions_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    utxos: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'utxos',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    utxos_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'utxos_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}
