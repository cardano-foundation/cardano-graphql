import { Resolvers } from '../graphql_types'
import { checkLimit } from '../validation'
import {
  block29021, block29022,
  epoch1,
  stakePool1,
  txe68043, tx05ad8b
} from '../lib/data_assertions'

export const mockedResolvers: Resolvers = {
  Mutation: {
    delegateStake: () => {
      return false
    },
    registerStakePool: () => {
      return false
    },
    submitTransaction: () => {
      return false
    }
  },
  Query: {
    blocks: (_root, args): any => {
      checkLimit(args.limit, 100)
      return [block29021, block29022]
    },
    epochs: (_root, args): any => {
      checkLimit(args.limit, 100)
      return [epoch1]
    },
    cardano: (): any => {
      return Promise.resolve({
        blockHeight: 99,
        currentEpoch: epoch1,
        configuration: {
          fees: {
            base: 155381,
            coefficient: 43946
          }
        },
        latestBlock: block29022,
        stakeDistribution: [stakePool1]
      })
    },
    stakePools: (_root, args) => {
      checkLimit(args.limit, 250)
      return Promise.resolve([stakePool1])
    },
    transactions: (_root, args): any => {
      checkLimit(args.limit, 250)
      return [tx05ad8b, txe68043]
    },
    utxoSet: (): any => {
      return tx05ad8b.outputs
    }
  }
}
