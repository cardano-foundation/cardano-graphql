import { GraphQLDateTime } from 'graphql-iso-date'
import {
  LoveLaces,
  Percentage,
  PublicKeyHash,
  StakePoolTicker,
  TransactionHash
} from '../lib/scalars'

const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime,
  Lovelaces: LoveLaces,
  Percentage: Percentage,
  PublicKeyHash: PublicKeyHash,
  StakePoolTicker: StakePoolTicker,
  TransactionHash: TransactionHash
} as any
