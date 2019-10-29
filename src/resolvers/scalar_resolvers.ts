import {
  Hash32HexString,
  LoveLaces,
  Percentage,
} from '../lib/scalars'

const GraphQLBigInt = require('graphql-bigint')
const GraphQLDateTime = require('graphql-type-datetime')

export const scalarResolvers = {
  Hash32HexString: Hash32HexString,
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime,
  Lovelaces: LoveLaces,
  Percentage: Percentage
} as any
