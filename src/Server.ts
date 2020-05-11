import { ApolloServer, ApolloServerExpressConfig } from 'apollo-server-express'
import * as express from 'express'
import corsMiddleware from 'cors'

export function Server (
  app: express.Application,
  apolloServerExpressConfig: ApolloServerExpressConfig,
  cors?: corsMiddleware.CorsOptions
): ApolloServer {
  const apolloServer = new ApolloServer(apolloServerExpressConfig)
  apolloServer.applyMiddleware({
    app,
    cors,
    path: '/'
  })
  return apolloServer
}
