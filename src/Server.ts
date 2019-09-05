import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer, ServerInfo } from 'apollo-server'
import * as depthLimit from 'graphql-depth-limit'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { resolvers } from './resolvers'

export type Config = {
  apiPort: number
  hasuraUri: string
  queryDepthLimit: number
  tracing: boolean
}

export function Server ({ apiPort, hasuraUri, queryDepthLimit, tracing }: Config) {
  let apolloServerInfo: ServerInfo
  return {
    async boot (): Promise<ServerInfo> {
      const hasura = await buildHasuraSchema(hasuraUri)
      const apolloServer = new ApolloServer({
        context: () => ({ hasura }),
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
