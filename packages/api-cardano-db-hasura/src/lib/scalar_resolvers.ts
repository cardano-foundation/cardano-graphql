import {
  Hash32HexString,
  LoveLaces,
  Percentage
} from './scalars'

const GraphQLBigInt = require('graphql-bigint')
const GraphQLDateTime = require('graphql-iso-date')

export const scalarResolvers = {
  Hash32HexString: Hash32HexString,
  BigInt: GraphQLBigInt,
  DateTime: GraphQLDateTime,
  Lovelaces: LoveLaces,
  Percentage: Percentage
} as any
