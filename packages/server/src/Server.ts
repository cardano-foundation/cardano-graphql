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
import { prometheusMetricsPlugin, queryComplexityPlugin } from './apollo_server_plugins'
import { IntrospectionNotPermitted, TracingRequired } from './errors'
import { allowListMiddleware } from './express_middleware'
import { dummyLogger, Logger } from 'ts-log'
import { setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'
import { clearIntervalAsync } from 'set-interval-async'
import { RunnableModuleState } from '@cardano-graphql/util'

export type Config = {
  allowIntrospection: boolean
  allowListPath?: string
  allowedOrigins?: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  listenAddress: string
  maxQueryComplexity?: number
  prometheusMetrics: boolean
  queryDepthLimit?: number
  tracing: boolean
}

export class Server {
  public app: express.Application
  public state: RunnableModuleState
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
    this.state = null
    this.schemas = schemas
  }

  async init () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'Server' }, 'Initializing')
    let allowList: AllowList
    const plugins: PluginDefinition[] = []
    const validationRules = []
    const schema = mergeSchemas({
      schemas: this.schemas
    })
    if (this.config.allowListPath) {
      if (this.config.allowIntrospection === true) {
        throw new IntrospectionNotPermitted('allowListPath')
      }
      try {
        const file = await fs.readFile(this.config.allowListPath, 'utf8')
        allowList = JSON.parse(file)
        this.app.use('/', json(), allowListMiddleware(allowList))
        this.logger.info({ module: 'Server' }, 'The server will only allow only operations from the provided list')
      } catch (error) {
        this.logger.error({ module: 'Server' }, `Cannot read or parse allow-list JSON file at ${this.config.allowListPath}`)
        throw error
      }
    }
    if (this.config.prometheusMetrics) {
      if (this.config.tracing === false) {
        throw new TracingRequired('Prometheus')
      }
      plugins.push(prometheusMetricsPlugin(this.app))
      this.logger.info({ module: 'Server' }, 'Prometheus metrics will be served at /metrics')
    }
    if (this.config.queryDepthLimit) {
      validationRules.push(depthLimit(this.config.queryDepthLimit))
    }
    if (this.config.maxQueryComplexity) {
      plugins.push(queryComplexityPlugin(schema, this.logger, this.config.maxQueryComplexity))
    }
    this.apolloServer = new ApolloServer({
      cacheControl: this.config.cacheEnabled ? { defaultMaxAge: 20 } : undefined,
      introspection: this.config.allowIntrospection,
      playground: this.config.allowIntrospection,
      plugins,
      validationRules,
      schema,
      tracing: this.config.tracing
    })
    this.apolloServer.applyMiddleware({
      app: this.app,
      cors: this.config.allowedOrigins ? {
        origin: this.config.allowedOrigins
      } : undefined,
      path: '/'
    })
    this.state = 'initialized'
  }

  async start () {
    if (this.state !== 'initialized') return
    this.httpServer = await listenPromise(this.app, this.config.apiPort, this.config.listenAddress)
    this.state = 'running'
    this.logger.info({ module: 'Server' }, `GraphQL HTTP server at http://${this.config.listenAddress}:` +
      `${this.config.apiPort}${this.apolloServer.graphqlPath} started`
    )
    this.logger.debug({ module: 'Server' }, 'Checking DB status')
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
        this.logger.debug({ module: 'Server' }, JSON.stringify(result.errors))
        return
      }
      if (result.data.cardanoDbMeta.initialized) {
        this.logger.info({ module: 'Server' }, 'DB ready')
        // Promise not awaited purposely
        // https://github.com/input-output-hk/cardano-graphql/issues/459
        clearIntervalAsync(this.syncProgress)
      } else {
        this.logger.info({ module: 'Server' }, `DB sync progress: ${result.data.cardanoDbMeta.syncPercentage} %`)
      }
    }, 5000)
  }

  async shutdown () {
    if (this.state !== 'running') return
    await clearIntervalAsync(this.syncProgress)
    this.httpServer.close()
    this.logger.info({ module: 'Server' }, `GraphQL HTTP server at http://${this.config.listenAddress}:` +
      `${this.config.apiPort}${this.apolloServer.graphqlPath} shutting down`
    )
    this.state = 'initialized'
  }
}
