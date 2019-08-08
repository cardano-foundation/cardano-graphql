import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Query: {
    transaction: (_root, args, { dataSources }) => dataSources.transactions.findById(args.id),
    transactions: (_root, args, { dataSources }) => dataSources.transactions.findByIds(args.ids)
  }
}
