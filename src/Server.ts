import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer } from 'apollo-server'
import { resolvers } from './resolvers'
import { Transactions } from './data'

export type Context = {
  dataSources: {
    transactions: Transactions
  }
}

export type Config = {
  dataSources: Context['dataSources']
  introspection?: boolean
  mocks?: boolean
}

export function Server ({ dataSources, introspection = true, mocks = false }: Config) {
  return new ApolloServer({
    dataSources (): Context['dataSources'] {
      return dataSources
    },
    introspection,
    mocks,
    resolvers,
    typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
  })
}
