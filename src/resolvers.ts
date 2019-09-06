// import { delegateToSchema } from 'graphql-tools'
import { GraphQLDateTime } from 'graphql-iso-date'
import { Resolvers } from './graphql_types'
import { GraphQLError } from 'graphql'
import {
  LoveLaces,
  Percentage,
  PublicKeyHash,
  StakePoolTicker,
  TransactionHash
} from './lib/scalars'
import {
  block43177, block43178,
  epoch2,
  stakePool1,
  txa54489, txd9e280, tx21c528
} from './lib/data_assertions'

const GraphQLBigInt = require('graphql-bigint')

const resolverMap: Resolvers = {
  Mutation: {
    delegateStake: (_root, _args, _context, _info) => {
      return false
    },
    registerStakePool: (_root, _args, _context, _info) => {
      return false
    },
    submitTransaction: (_root, _args, _context, _info) => {
      return false
    }
  },
  Query: {
    blocks: (_root, args, _context, _info) => {
      checkLimit(args.limit, 100)
      return [block43177, block43178]
      // return delegateToSchema({
      //   args,
      //   context,
      //   fieldName: 'blocks',
      //   info,
      //   operation: 'query',
      //   schema: context.hasura
      // })
    },
    epochs: (_root, args, _context, _info) => {
      checkLimit(args.limit, 100)
      return [epoch2]
      // return delegateToSchema({
      //   args,
      //   context,
      //   fieldName: 'transactions',
      //   info,
      //   operation: 'query',
      //   schema: context.hasura
      // })
    },
    cardano: (_root, _args, _context, _info) => {
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
    stakePools: (_root, args, _context, _info) => {
      checkLimit(args.limit, 250)
      return Promise.resolve([stakePool1])
    },
    transactions: (_root, args, _context, _info) => {
      checkLimit(args.limit, 250)
      return [tx21c528, txa54489, txd9e280]
      // return delegateToSchema({
      //   args,
      //   context,
      //   fieldName: 'transactions',
      //   info,
      //   operation: 'query',
      //   schema: context.hasura
      // })
    },
    utxoSet: (_root, _args, _context, _info) => {
      return txd9e280.outputs
      // return delegateToSchema({
      //   args: { where: { address: { _eq: args.address } } },
      //   context,
      //   fieldName: 'utxo',
      //   info,
      //   operation: 'query',
      //   schema: context.hasura
      // })
    }
  }
}

export const resolvers = Object.assign({}, resolverMap, {
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime,
  Lovelaces: LoveLaces,
  Percentage: Percentage,
  PublicKeyHash: PublicKeyHash,
  StakePoolTicker: StakePoolTicker,
  TransactionHash: TransactionHash
}) as any

function checkLimit (requested: number, maxAllowed: number): void {
  if (requested < 0) throw new GraphQLError('Limit must be a positive integer')
  if (requested > maxAllowed) {
    throw new GraphQLError(
      `${requested} exceeds the maximum allowed value of ${maxAllowed}. Use the offset to paginate through a larger result set`
    )
  }
}
