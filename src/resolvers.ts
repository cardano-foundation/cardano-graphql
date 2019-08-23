import { GraphQLDateTime } from 'graphql-iso-date'
import { Resolvers, Transaction } from './graphql_types'

const resolverMap: Resolvers = {
  Query: {
    // blocks: (_root, args, { dataSources: { ledger } }) => ledger.block.loadMany(args.ids),
    // epochs: (_root, args, { dataSources: { ledger } }) => ledger.epoch.loadMany(args.numbers),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transactions(args) as unknown as Promise<Transaction[]>
    // utxo: (_root, args, { dataSources: { ledger } }) => ledger.utxo.load(args.address)
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  DateTime: GraphQLDateTime
})
