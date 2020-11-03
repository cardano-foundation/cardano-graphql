import fs from 'fs'
import path from 'path'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { Resolvers } from './graphql_types'

export function buildSchema () {
  return makeExecutableSchema({
    resolvers: {
      Query: {
        test: () => 'foo',
        testTwo: () => 'bar'
      }
    } as Resolvers,
    typeDefs: fs.readFileSync(path.resolve(__dirname, '..', 'schema.graphql'), 'utf-8')
  })
}
