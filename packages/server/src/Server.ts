import { PluginDefinition } from 'apollo-server-core'
import { ApolloServer, CorsOptions } from 'apollo-server-express'
import { json } from 'body-parser'
import express from 'express'
import fs from 'fs-extra'
import { GraphQLSchema } from 'graphql'
import depthLimit from 'graphql-depth-limit'
import { mergeSchemas } from '@graphql-tools/merge'
import http from 'http'
import { listenPromise } from './util'
import { AllowList } from './AllowList'
import { prometheusMetricsPlugin } from './apollo_server_plugins'
import { IntrospectionNotPermitted, TracingRequired } from './errors'
import { allowListMiddleware } from './express_middleware'
import { dummyLogger, Logger } from 'ts-log'
import { setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'
import { clearIntervalAsync } from 'set-interval-async'

export type Config = {
  allowIntrospection: boolean
  allowListPath?: string
  allowedOrigins?: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  listenAddress: string
  prometheusMetrics: boolean
  queryDepthLimit?: number
  tracing: boolean
}

export class Server {
  public app: express.Application
  private apolloServer: ApolloServer
  private httpServer: http.Server
  private schemas: GraphQLSchema[]
  private syncProgress: SetIntervalAsyncTimer

  constructor (
    schemas: GraphQLSchema[],
    private config: Config,
    private logger: Logger = dummyLogger
  ) {
    this.app = express()
    this.schemas = schemas
  }

  async init () {
    let allowList: AllowList
    const plugins: PluginDefinition[] = []
    const validationRules = []
    if (this.config.allowListPath) {
      if (this.config.allowIntrospection === true) {
        throw new IntrospectionNotPermitted('allowListPath')
      }
      try {
        const file = await fs.readFile(this.config.allowListPath, 'utf8')
        allowList = JSON.parse(file)
        this.app.use('/', json(), allowListMiddleware(allowList))
        this.logger.info('The server will only allow only operations from the provided list')
      } catch (error) {
        this.logger.error(`Cannot read or parse allow-list JSON file at ${this.config.allowListPath}`)
        throw error
      }
    }
    if (this.config.prometheusMetrics) {
      if (this.config.tracing === false) {
        throw new TracingRequired('Prometheus')
      }
      plugins.push(prometheusMetricsPlugin(this.app))
      this.logger.info('Prometheus metrics will be served at /metrics')
    }
    if (this.config.queryDepthLimit) {
      validationRules.push(depthLimit(this.config.queryDepthLimit))
    }
    this.apolloServer = new ApolloServer({
      cacheControl: this.config.cacheEnabled ? { defaultMaxAge: 20 } : undefined,
      introspection: this.config.allowIntrospection,
      playground: this.config.allowIntrospection,
      plugins,
      validationRules,
      schema: mergeSchemas({
        schemas: this.schemas
      }),
      tracing: this.config.tracing
    })
    this.apolloServer.applyMiddleware({
      app: this.app,
      cors: this.config.allowedOrigins ? {
        origin: this.config.allowedOrigins
      } : undefined,
      path: '/'
    })
  }

  async start () {
    this.httpServer = await listenPromise(this.app, this.config.apiPort, this.config.listenAddress)
    this.logger.info(`GraphQL HTTP server at http://${this.config.listenAddress}:` +
      `${this.config.apiPort}${this.apolloServer.graphqlPath} started`
    )
    this.logger.debug('Checking DB status', { module: 'Server' })
    this.syncProgress = setIntervalAsync(async () => {
      const result = await this.apolloServer.executeOperation(
        {
          query: `query getSyncStatus {
              cardanoDbMeta {
                  initialized
                  syncPercentage
              }
          }`
        }
      )
      if (result.errors !== undefined) {
        this.logger.debug(JSON.stringify(result.errors))
        return
      }
      if (result.data.cardanoDbMeta.initialized) {
        this.logger.info('DB ready')
        await clearIntervalAsync(this.syncProgress)
      } else {
        this.logger.info(`DB sync progress: ${result.data.cardanoDbMeta.syncPercentage} %`)
      }
    }, 5000)
  }

  async shutdown () {
    await clearIntervalAsync(this.syncProgress)
    if (this.httpServer !== undefined) {
      this.httpServer.close()
      this.logger.info(`GraphQL HTTP server at http://${this.config.listenAddress}:` +
        `${this.config.apiPort}${this.apolloServer.graphqlPath} shutting down`
      )
    }
  }
}
