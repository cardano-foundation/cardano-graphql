import { GraphQLDateTime } from 'graphql-iso-date'
import { Resolvers, Transaction } from './graphql_types'
const GraphQLBigInt = require('graphql-bigint')

const resolverMap: Resolvers = {
  Block: {
    transactions: async (parent, args, { dataSources: { ledger } }) => {
      return ledger.transactionRepository.byBlockNumbers([parent.number], args.limit) as unknown as Promise<Transaction[]>
    }
  },
  Query: {
    blocks: (_root, args, { dataSources: { ledger } }) => ledger.blocks(args),
    ledger: (_root, _args, { dataSources: { ledger } }) => ledger.ledger(),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transactions(args)
    // utxo: (_root, args, { dataSources: { ledger } }) => ledger.utxo.load(args.address)
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime
})
