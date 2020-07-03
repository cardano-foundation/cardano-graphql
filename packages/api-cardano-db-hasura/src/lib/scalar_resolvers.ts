import {
  DateTime,
  Hash32HexString,
  Lovelaces,
  Percentage
} from './scalars'

const GraphQLBigInt = require('graphql-bigint')

export const scalarResolvers = {
  Hash32HexString: Hash32HexString,
  BigInt: GraphQLBigInt,
  DateTime,
  Lovelaces,
  Percentage: Percentage
} as any
