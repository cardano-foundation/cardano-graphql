import { delegateToSchema } from 'graphql-tools'
import { GraphQLDateTime } from 'graphql-iso-date'
import { Resolvers } from './graphql_types'
import { GraphQLError } from 'graphql'

const GraphQLBigInt = require('graphql-bigint')

const resolverMap: Resolvers = {
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
    ledgerStats: (_root, _args, _context, _info) => {
      // Todo: Implement view, maybe LedgerStats
      // const res = delegateToSchema({
      //   context,
      //   fieldName: 'ledgerStats',
      //   info,
      //   operation: 'query',
      //   schema: context.hasura,
      // })
      return { blockHeight: 99 }
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
    utxo: (_root, args, context, info) => {
      return delegateToSchema({
        args: { where: { address: { _eq: args.address } } },
        context,
        fieldName: 'utxo',
        info,
        operation: 'query',
        schema: context.hasura
      })
    }
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime
}) as any

function checkLimit (requested: number, maxAllowed: number): void {
  if (requested < 0) throw new GraphQLError('Limit must be a positive integer')
  if (requested > maxAllowed) {
    throw new GraphQLError(
      `${requested} exceeds the maximum allowed value of ${maxAllowed}. Use the offset to paginate through a larger result set`
    )
  }
}
