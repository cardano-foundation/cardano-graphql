import * as fs from 'fs'
import * as path from 'path'
import { Connection, ConnectionManager } from 'typeorm'
import { ApolloServer, ServerInfo } from 'apollo-server'
import { Context } from './Context'
import { Ledger,
  BlockDataModel,
  TxDataModel,
  TxInDataModel,
  TxOutDataModel
} from './data_sources/ledger'

import { resolvers } from './resolvers'

export type Config = {
  apiPort: number
  tracing: boolean
  postgres: {
    database: string
    host: string
    password: string
    port: number
    username: string
  }
}

export function Server (config: Config) {
  const connectionManager = new ConnectionManager()
  const pgConnection: Connection = connectionManager.create({
    ...config.postgres,
    type: 'postgres',
    entities: [BlockDataModel, TxDataModel, TxInDataModel, TxOutDataModel]
  })
  const apolloServer = new ApolloServer({
    dataSources (): Context['dataSources'] {
      return {
        ledger: new Ledger({
          transactions: pgConnection.getRepository(TxDataModel)
        })
      }
    },
    introspection: true,
    resolvers,
    tracing: config.tracing,
    typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
  })
  let apolloServerInfo: ServerInfo
  return {
    async boot (): Promise<ServerInfo> {
      await pgConnection.connect()
      apolloServerInfo = await apolloServer.listen({ port: config.apiPort })
      return apolloServerInfo
    },
    async shutdown (): Promise<void> {
      await Promise.all([
        pgConnection.close(),
        apolloServerInfo.server.close()
      ])
    }
  }
}
