import * as apolloServerPlugins from './apollo_server_plugins'
import express from 'express'
import fs from 'fs'
import { getConfig } from './config'
import { Server } from './Server'
import depthLimit from 'graphql-depth-limit'
import { mergeSchemas } from '@graphql-tools/merge'
import { PluginDefinition } from 'apollo-server-core'
import { buildSchema as buildLedgerSchema } from '@cardano-graphql/api-cardano-db-hasura'
export * from './config'
export { apolloServerPlugins }

const config = getConfig()
const { prometheusMetricsPlugin, whitelistPlugin } = apolloServerPlugins

buildLedgerSchema(config.hasuraUri).then((ledgerSchema) => {
  const app = express()
  let plugins: PluginDefinition[]
  const validationRules = []

  if (config.prometheusMetrics) {
    plugins = [prometheusMetricsPlugin(app)]
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
    introspection: config.allowIntrospection,
    playground: config.allowIntrospection,
    plugins,
    validationRules,
    schema: mergeSchemas({ schemas: [ledgerSchema] })
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
