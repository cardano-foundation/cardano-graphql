import * as fs from 'fs'
import * as path from 'path'
import { ApolloServer, CorsOptions } from 'apollo-server-express'
import * as express from 'express'
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

export function Server ({ cacheEnabled, context, allowedOrigins, queryDepthLimit, resolvers, tracing }: Config): express.Application {
  const app = express()
  const apolloServer = new ApolloServer({
    cacheControl: cacheEnabled ? { defaultMaxAge: 20 } : undefined,
    context,
    introspection: true,
    resolvers,
    tracing,
    typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8'),
    validationRules: [depthLimit(queryDepthLimit)]
  })
  apolloServer.applyMiddleware({
    app,
    cors: {origin: allowedOrigins}
  })
  return app
}
