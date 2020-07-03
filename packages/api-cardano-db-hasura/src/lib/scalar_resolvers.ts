import {
  DateTime,
  Hash32HexString,
  LoveLaces,
  Percentage
} from './scalars'

const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  Hash32HexString: Hash32HexString,
  BigInt: GraphQLBigInt,
  DateTime: DateTime,
  Lovelaces: LoveLaces,
  Percentage: Percentage
} as any
