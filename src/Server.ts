import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer, CorsOptions, ServerInfo } from 'apollo-server'
import * as depthLimit from 'graphql-depth-limit'
import { Resolvers } from './graphql_types'
import { Context } from './Context'

export type Config = {
  allowedOrigins: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  context: () => Context | void
  queryDepthLimit: number
  resolvers: Resolvers
  tracing: boolean
}

export function Server ({ apiPort, cacheEnabled, context, allowedOrigins, queryDepthLimit, resolvers, tracing }: Config) {
  let apolloServerInfo: ServerInfo
  return {
    async boot (): Promise<ServerInfo> {
      const apolloServer = new ApolloServer({
        cacheControl: cacheEnabled ? { defaultMaxAge: 20 } : undefined,
        context,
        cors: { origin: allowedOrigins },
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
