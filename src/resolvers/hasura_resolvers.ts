import { delegateToSchema } from 'graphql-tools'
import { Resolvers } from '../graphql_types'

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: async (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Block',
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
    cardano: async (_root, args, context, info) => {
      return (await delegateToSchema({
        args,
        context,
        fieldName: 'Cardano',
        info,
        operation: 'query',
        schema: context.hasura
      }))[0]
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
    utxoSet: (_root, args, context, info) => {
      return delegateToSchema({
        args,
        context,
        fieldName: 'Utxo',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}
