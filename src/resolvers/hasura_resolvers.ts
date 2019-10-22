import { delegateToSchema } from 'graphql-tools'
import { Resolvers, Order_By_With_Nulls as OrderByWithNulls } from '../graphql_types'
import { checkLimit } from '../validation'
import { GraphQLError } from 'graphql'
const isEqual = require('lodash.isequal')

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: async (_root, args, context, info) => {
      checkLimit(args.limit, 100)
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
      // The arg properties weirdly extend the null prototype,
      // so implicit falsy checks don't behave as expected
      if (args.where === undefined || args.where.number === undefined) {
        throw new GraphQLError('number must be specified (_eq) or bounded (_in)')
      }

      const hasEqArg = args.where.number._eq !== undefined
      const hasRange = args.where.number._in !== undefined

      if (!hasEqArg && !hasRange) {
        throw new GraphQLError('number must be specified (_eq) or bounded (_in)')
      }

      if (hasRange && args.where.number._in.length > 10) {
        throw new GraphQLError('Maximum number of epochs queryable in a range is 10')
      }

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
      checkLimit(args.limit, 250)
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
      checkLimit(args.limit, 250)
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
