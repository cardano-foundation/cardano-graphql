import * as express from 'express'
import * as fs from 'fs'
import * as path from 'path'
import { getConfig } from './config'
import { buildContext, Context } from './Context'
import { prometheusMetricsPlugin, whitelistPlugin } from './apollo_server_plugins'
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
    if (config.whitelistPath) {
      const whitelist = JSON.parse(fs.readFileSync(config.whitelistPath, 'utf8'))
      plugins.push(whitelistPlugin(whitelist))
    }
    if (config.queryDepthLimit) {
      validationRules.push(depthLimit(config.queryDepthLimit))
    }
    const server = Server(app, {
      cacheControl: config.cacheEnabled ? { defaultMaxAge: 20 } : undefined,
      context,
      introspection: config.allowIntrospection,
      playground: config.allowIntrospection,
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
        if (process.env.NODE_ENV !== 'production' && config.whitelistPath) {
          console.warn('As a whitelist is in effect, the GraphQL Playground is available, but will not allow schema exploration')
        }
        if (config.prometheusMetrics) {
          console.log(`Prometheus metrics at ${serverUri}/metrics`)
        }
      })
    } catch (error) {
      console.error(error.message)
    }
  })
