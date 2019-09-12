import { Resolvers } from '../graphql_types'
import { checkLimit } from '../validation'
import {
  block43177, block43178,
  epoch2,
  stakePool1,
  txa54489, txd9e280, tx21c528
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
    blocks: (_root, args) => {
      checkLimit(args.limit, 100)
      return [block43177, block43178]
    },
    epochs: (_root, args) => {
      checkLimit(args.limit, 100)
      return [epoch2]
    },
    cardano: () => {
      return Promise.resolve({
        blockHeight: 99,
        currentEpoch: epoch2,
        configuration: {
          fees: {
            base: 155381,
            coefficient: 43946
          }
        },
        latestBlock: block43178,
        stakeDistribution: [stakePool1]
      })
    },
    stakePools: (_root, args) => {
      checkLimit(args.limit, 250)
      return Promise.resolve([stakePool1])
    },
    transactions: (_root, args) => {
      checkLimit(args.limit, 250)
      return [tx21c528, txa54489, txd9e280]
    },
    utxoSet: () => {
      return txd9e280.outputs
    }
  }
}
