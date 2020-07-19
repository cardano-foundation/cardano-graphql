import fs from 'fs'
import path from 'path'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { JSONObjectResolver } from 'graphql-scalars'
import { Resolvers, Genesis } from './graphql_types'

export const scalarResolvers = {
  JSONObject: JSONObjectResolver
} as any

export function buildSchema (genesis: Genesis) {
  return makeExecutableSchema({
    resolvers: Object.assign({}, scalarResolvers, {
      Query: {
        genesis: async () => genesis
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
