import * as express from 'express'
import * as fs from 'fs'
import * as path from 'path'
import { getConfig } from './config'
import { buildContext, Context } from './Context'
import { prometheusMetricsPlugin } from './apollo_server_plugins'
import * as depthLimit from 'graphql-depth-limit'
import { Server } from './Server'
import resolvers from './resolvers'

const config = getConfig()

buildContext(config.hasuraUri)
  .then((context: Context) => {
    const app = express()
    const plugins = []
    const validationRules = []

    if (config.prometheusMetrics) {
      plugins.push(prometheusMetricsPlugin(app))
    }
    if (config.queryDepthLimit) {
      validationRules.push(depthLimit(config.queryDepthLimit))
    }
    const server = Server(app, {
      cacheControl: config.cacheEnabled ? { defaultMaxAge: 20 } : undefined,
      context,
      introspection: true,
      plugins,
      resolvers,
      validationRules,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
    }, {
      origin: config.allowedOrigins
    })

    try {
      app.listen({ port: config.apiPort }, () => {
        const serverUri = `http://localhost:${config.apiPort}`
        console.log(`GraphQL HTTP server at ${serverUri}${server.graphqlPath}`)
        if (config.prometheusMetrics) {
          console.log(`Prometheus metrics at ${serverUri}/metrics`)
        }
      })
    } catch (error) {
      console.error(error.message)
    }
  })
