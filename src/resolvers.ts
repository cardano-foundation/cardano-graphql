import { GraphQLDateTime } from 'graphql-iso-date'
import { Resolvers } from './graphql_types'

const resolverMap: Resolvers = {
  Query: {
    block: (_root, args, { dataSources: { ledger } }) => ledger.block.load(args.id),
    blocks: (_root, args, { dataSources: { ledger } }) => ledger.block.loadMany(args.ids),
    epoch: (_root, args, { dataSources: { ledger } }) => ledger.epoch.load(args.number),
    epochs: (_root, args, { dataSources: { ledger } }) => ledger.epoch.loadMany(args.numbers),
    transaction: (_root, args, { dataSources: { ledger } }) => ledger.transaction.load(args.id),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transaction.loadMany(args.ids),
    utxo: (_root, args, { dataSources: { ledger } }) => ledger.utxo.load(args.address)
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  DateTime: GraphQLDateTime
})
