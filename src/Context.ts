import { GraphQLSchema } from 'graphql'
import { StakePoolMetadataRepository } from './StakePoolMetadataRepository'

export type Context = {
  hasura?: GraphQLSchema
  stakePoolMetaRepo?: ReturnType<typeof StakePoolMetadataRepository >
}
