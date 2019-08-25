import * as fs from 'fs'
import * as path from 'path'
import { Connection } from 'typeorm'
import { ApolloServer, ServerInfo } from 'apollo-server'
import { Context } from './Context'
import { Ledger, TxDataModel } from './data_sources/ledger'

import { resolvers } from './resolvers'

export type Config = {
  apiPort: number
  tracing: boolean
  postgres: Connection
}

export function Server ({ apiPort, tracing, postgres }: Config) {
  const apolloServer = new ApolloServer({
    dataSources (): Context['dataSources'] {
      return {
        ledger: new Ledger({
          transactions: postgres.getRepository(TxDataModel)
        })
      }
    },
    introspection: true,
    resolvers,
    tracing,
    typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
  })
  let apolloServerInfo: ServerInfo
  return {
    async boot (): Promise<ServerInfo> {
      await postgres.connect()
      apolloServerInfo = await apolloServer.listen({ port: apiPort })
      return apolloServerInfo
    },
    async shutdown (): Promise<void> {
      await Promise.all([
        postgres.close(),
        apolloServerInfo.server.close()
      ])
    }
  }
}
