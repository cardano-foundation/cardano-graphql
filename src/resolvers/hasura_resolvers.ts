import { delegateToSchema } from 'graphql-tools'
import { GraphQLError } from 'graphql'
import { Resolvers, Order_By_With_Nulls } from '../graphql_types'
import { checkLimit } from '../validation'
const isEqual = require('lodash.isequal')

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: async (_root, args, context, info) => {
      checkLimit(args.limit, 100)
      return delegateToSchema({
        args: isEqual(args, { limit: 1 }) ? { ...args, ...{ order_by: { number: Order_By_With_Nulls.DescNullsLast }}} : args,
        context,
        fieldName: 'Block',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    epochs: (_root, args, context, info) => {
      checkLimit(args.limit, 100)
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
    stakePools: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
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
      return delegateToSchema({
        args: { where: { address: { _eq: args.where.address } } },
        context,
        fieldName: 'Utxo',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}
