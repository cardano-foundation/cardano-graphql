import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Query: {
    transaction: (_root, args, { dataSources: { ledger } }) => ledger.transaction.load(args.id),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transaction.loadMany(args.ids)
  }
}
