import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer, ServerInfo } from 'apollo-server'
import * as depthLimit from 'graphql-depth-limit'
import { Resolvers } from './graphql_types'

export type Config = {
  apiPort: number
  context: () => any
  queryDepthLimit: number
  resolvers: Resolvers
  tracing: boolean
}

export function Server ({ apiPort, context, queryDepthLimit, resolvers, tracing }: Config) {
  let apolloServerInfo: ServerInfo
  return {
    async boot (): Promise<ServerInfo> {
      const apolloServer = new ApolloServer({
        context,
        introspection: true,
        resolvers,
        tracing,
        typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8'),
        validationRules: [depthLimit(queryDepthLimit)]
      })
      apolloServerInfo = await apolloServer.listen({ port: apiPort })
      return apolloServerInfo
    },
    async shutdown (): Promise<void> {
      await apolloServerInfo.server.close()
    }
  }
}
