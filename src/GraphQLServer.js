import express from 'express'
import { ApolloServer } from 'apollo-server-express'
import https from 'https'
import http from 'http'

export default class GraphQLServer {
  schema = null
  certificateService = null
  tlsEnabled = null
  apolloServer = null
  httpServer = null

  constructor(schema, options = { tlsEnabled: false, certificateService: null }) {
    this.schema = schema
    this.certificateService = options.certificateService
    this.tlsEnabled = options.tlsEnabled
    this.apolloServer = new ApolloServer({
      schema,
      introspection: true
    })
    const app = express()
    this.apolloServer.applyMiddleware({ app })
    if (this.tlsEnabled && this.certificateService) {
      this.httpServer = https.createServer({
        ca: this.certificateService.caStore,
        cert: this.certificateService.certPem,
        key: this.certificateService.privateKeyPem,
        requestCert: true,
        rejectUnauthorized: false
      }, app)
    } else {
      this.httpServer = http.createServer(app)
    }
    this.apolloServer.installSubscriptionHandlers(this.httpServer)
  }

  listen(...args) {
    return this.httpServer.listen(...args)
  }

  endpoint(port) {
    const protocol = this.tlsEnabled ? 'https' : 'http'
    return {
      graphqlPath: `${protocol}://localhost:${port}${this.graphqlPath}`,
      subscriptionsPath: `ws://localhost:${port}${this.subscriptionsPath}`
    }
  }

  get graphqlPath() {
    return this.apolloServer.graphqlPath
  }

  get subscriptionsPath() {
    return this.apolloServer.subscriptionsPath
  }

  shutdown(callback) {
    this.httpServer.close(callback)
  }

}
