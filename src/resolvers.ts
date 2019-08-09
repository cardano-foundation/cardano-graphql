import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Query: {
    transaction: (_root, args, { dataSources: { ledger } }) => ledger.transaction(args.id),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transactions(args.ids)
  }
}
