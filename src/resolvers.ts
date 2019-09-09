// import { delegateToSchema } from 'graphql-tools'
import { DateTimeResolver, PositiveIntResolver } from 'graphql-scalars'
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
  txa54489, txd9e280, tx21c528
} from './lib/data_assertions'

import { generateStakeDelegations, generateStakeholders, generateStakepools } from './lib/mocks'

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
        stakeDistribution: generateStakepools(30)
      })
    },
    stakeholders: (_root, args, _context, _info) => {
      checkLimit(args.limit, 250)
      return Promise.resolve(generateStakeholders(args.limit))
    },
    stakeDelegations: (_root, args, _context, _info) => {
      checkLimit(args.limit, 250)
      return Promise.resolve(generateStakeDelegations(args.limit))
    },
    stakePools: (_root, args, _context, _info) => {
      checkLimit(args.limit, 250)
      return Promise.resolve(generateStakepools(args.limit))
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
    utxoSet: () => {
      return [...txd9e280.outputs, ...txa54489.outputs, ...tx21c528.outputs]
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
  DateTime: DateTimeResolver,
  Lovelaces: LoveLaces,
  Percentage: Percentage,
  PublicKeyHash: PublicKeyHash,
  StakePoolTicker: StakePoolTicker,
  TransactionHash: TransactionHash,
  PositiveInt: PositiveIntResolver
}) as any

function checkLimit (requested: number, maxAllowed: number): void {
  if (requested < 0) throw new GraphQLError('Limit must be a positive integer')
  if (requested > maxAllowed) {
    throw new GraphQLError(
      `${requested} exceeds the maximum allowed value of ${maxAllowed}. Use the offset to paginate through a larger result set`
    )
  }
}
