import { delegateToSchema } from 'graphql-tools'
import { Resolvers, Order_By_With_Nulls as OrderByWithNulls } from '../graphql_types'
import { validateQueryArgs } from '../validation'
const isEqual = require('lodash.isequal')

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: async (_root, args, context, info) => {
      validateQueryArgs(args)
      return delegateToSchema({
        args: isEqual(args, { limit: 1 }) ? { ...args, ...{ order_by: { number: OrderByWithNulls.DescNullsLast } } } : args,
        context,
        fieldName: 'Block',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs: (_root, args, context, info) => {
      validateQueryArgs(args)
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
      validateQueryArgs(args)
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
      validateQueryArgs(args)
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
