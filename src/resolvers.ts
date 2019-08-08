import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Query: {
    mempool: async (_root, _args, { dataSources: { mempool } }) => ({
      transactions: await mempool.transactions(),
      transactionCount: await mempool.transactionCount()
    }),
    transaction: (_root, args, { dataSources: { transactions } }) => transactions.findById(args.id),
    transactions: (_root, args, { dataSources: { transactions } }) => transactions.findByIds(args.ids)
  }
}
