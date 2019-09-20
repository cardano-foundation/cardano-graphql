import { delegateToSchema } from 'graphql-tools'
import { GraphQLError } from 'graphql'
import { Resolvers } from '../graphql_types'
import { checkLimit } from '../validation'

export const hasuraResolvers: Resolvers = {
  Query: {
    blocks: (_root, args, context, info) => {
      checkLimit(args.limit, 100)
      return delegateToSchema({
        args,
        context,
        fieldName: 'blocks',
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
        fieldName: 'transactions',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    cardano: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    stakePools: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    transactions: (_root, args, context, info) => {
      checkLimit(args.limit, 250)
      return delegateToSchema({
        args,
        context,
        fieldName: 'transactions',
        info,
        operation: 'query',
        schema: context.hasura
      })
    },
    utxoSet: (_root, args, context, info) => {
      return delegateToSchema({
        args: { where: { address: { _eq: args.where.address } } },
        context,
        fieldName: 'utxo',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}
