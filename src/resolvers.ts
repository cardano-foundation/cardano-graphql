import { GraphQLDateTime } from 'graphql-iso-date'
import { Block, Resolvers, Transaction } from './graphql_types'
const GraphQLBigInt = require('graphql-bigint')

const resolverMap: Resolvers = {
  Block: {
    transactions: async (parent, args, { dataSources: { ledger } }) => {
      return ledger.transactionRepository.byBlockNumbers([parent.number], args.limit) as unknown as Promise<Transaction[]>
    }
  },
  Query: {
    blocks: (_root, args, { dataSources: { ledger } }) => ledger.blocks(args) as unknown as Promise<Block[]>,
    ledger: (_root, _args, { dataSources: { ledger } }) => ledger.blockHeight(),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transactions(args) as unknown as Promise<Transaction[]>
    // utxo: (_root, args, { dataSources: { ledger } }) => ledger.utxo.load(args.address)
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime
})
