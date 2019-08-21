import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Query: {
    block: (_root, args, { dataSources: { ledger } }) => ledger.blockById(args.id),
    transaction: (_root, args, { dataSources: { ledger } }) => ledger.transaction.load(args.id),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transaction.loadMany(args.ids)
  }
}
