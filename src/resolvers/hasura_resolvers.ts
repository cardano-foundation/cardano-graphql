import { delegateToSchema } from 'graphql-tools'
import { Resolvers } from '../graphql_types'

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Block',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    blocks_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Block_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Epoch',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Epoch_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    cardano: async (_root, _args, context, info) => {
      // These two queries are very lightweight, just selecting single rows,
      // Could optimise, but there's little performance gain
      const cardanoResult = await delegateToSchema({
        context,
        fieldName: 'Cardano',
        info,
        operation: 'query',
        schema: context.hasura
      })
      const metaResult = await delegateToSchema({
        context,
        fieldName: 'Meta',
        info,
        operation: 'query',
        schema: context.hasura
      })
      return {
        ...cardanoResult ? cardanoResult[0] : undefined,
        ...metaResult ? metaResult[0] : undefined
      }
    },
    transactions: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Transaction',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    transactions_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Transaction_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    utxos: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Utxo',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    utxos_aggregate: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Utxo_aggregate',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}
