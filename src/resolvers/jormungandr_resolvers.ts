import { GraphQLError } from 'graphql'
import { Resolvers } from '../graphql_types'
import { checkLimit } from '../validation'

export const jormungandrResolvers: Resolvers = {
  Mutation: {
    delegateStake: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    registerStakePool: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    submitTransaction: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    }
  },
  Query: {
    blocks: (_root, args) => {
      checkLimit(args.limit, 100)
      throw new GraphQLError('Not implemented')
    },
    epochs: (_root, args) => {
      checkLimit(args.limit, 100)
      throw new GraphQLError('Not implemented')
    },
    cardano: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    stakePools: (_root, _args, _context, _info) => {
      throw new GraphQLError('Not implemented')
    },
    transactions: (_root, args) => {
      checkLimit(args.limit, 250)
      throw new GraphQLError('Not implemented')
    },
    utxoSet: () => {
      throw new GraphQLError('Not implemented')
    }
  }
}
