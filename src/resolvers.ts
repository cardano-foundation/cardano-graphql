import { Resolvers } from './graphql_types'

export const resolvers: Resolvers = {
  Entity: {
    // obj should be typed correctly, but the compiler is complaining
    // https://github.com/input-output-hk/cardano-graphql/issues/12
    __resolveType: (obj: any) => {
      if(obj.fee) return 'Transaction'
    }
  },
  Query: {
    transaction: (_root, args, { dataSources: { ledger } }) => ledger.transaction.load(args.id),
    transactions: (_root, args, { dataSources: { ledger } }) => ledger.transaction.loadMany(args.ids)
  }
}
