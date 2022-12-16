import { Config as ApiCardanoDbHasuraConfig } from '@cardano-graphql/api-cardano-db-hasura'
import { MissingConfig } from './errors'
import { Config as ServerConfig } from './Server'
import { LogLevelString } from 'bunyan'

export type Config = ServerConfig & ApiCardanoDbHasuraConfig & {
  loggerMinSeverity: LogLevelString
}

export async function getConfig (): Promise<Config> {
  const env = filterAndTypecastEnvs(process.env)

  if (!env.cardanoNodeConfigPath) {
    throw new MissingConfig('CARDANO_NODE_CONFIG_PATH env not set')
  }
  if (!env.hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
  }
  return {
    ...env,
    allowIntrospection:
      (process.env.NODE_ENV === 'production' && env.allowIntrospection) ||
      (process.env.NODE_ENV !== 'production' && env.allowIntrospection !== false),
    allowedOrigins: env.allowedOrigins || true,
    apiPort: env.apiPort || 3100,
    cacheEnabled: env.cacheEnabled || false,
    loggerMinSeverity: env.loggerMinSeverity || 'info' as LogLevelString,
    listenAddress: env.listenAddress || '0.0.0.0',
    maxQueryComplexity: env.maxQueryComplexity,
    pollingInterval: {
      adaSupply: env.pollingInterval.adaSupply || 1000 * 60
    },
    queryDepthLimit: env.queryDepthLimit || 10
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    ALLOW_LIST_PATH,
    API_PORT,
    CACHE_ENABLED,
    CARDANO_NODE_CONFIG_PATH,
    HASURA_URI,
    LISTEN_ADDRESS,
    LOGGER_MIN_SEVERITY,
    MAX_QUERY_COMPLEXITY,
    OGMIOS_HOST,
    OGMIOS_PORT,
    POLLING_INTERVAL_ADA_SUPPLY,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env as NodeJS.ProcessEnv
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true',
    allowedOrigins: ALLOWED_ORIGINS,
    allowListPath: ALLOW_LIST_PATH,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true',
    cardanoNodeConfigPath: CARDANO_NODE_CONFIG_PATH,
    hasuraUri: HASURA_URI,
    listenAddress: LISTEN_ADDRESS,
    loggerMinSeverity: LOGGER_MIN_SEVERITY as LogLevelString,
    maxQueryComplexity: Number(MAX_QUERY_COMPLEXITY),
    ogmios: {
      host: OGMIOS_HOST,
      port: OGMIOS_PORT ? Number(OGMIOS_PORT) : undefined
    },
    pollingInterval: {
      adaSupply: Number(POLLING_INTERVAL_ADA_SUPPLY)
    },
    prometheusMetrics: PROMETHEUS_METRICS === 'true',
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true'
  }
}
