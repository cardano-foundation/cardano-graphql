import { IntrospectionNotPermitted, MissingConfig, TracingRequired } from './errors'
import { Config as ServerConfig } from './Server'

export type Config = ServerConfig & {
  genesisFileByron: string
  genesisFileShelley: string
  hasuraUri: string
}

export async function getConfig (): Promise<Config> {
  const {
    allowIntrospection,
    allowedOrigins,
    allowListPath,
    apiPort,
    cacheEnabled,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  if (!hasuraUri && !genesisFileShelley) {
    throw new MissingConfig(
      `You have not provided configuration to load an API segment. Either set HASURA_URI or 
      GENESIS_FILE_SHELLEY`
    )
  }
  if (prometheusMetrics && process.env.TRACING === 'false') {
    throw new TracingRequired('Prometheus')
  }
  if (allowListPath && allowIntrospection) {
    throw new IntrospectionNotPermitted('allowList')
  }
  return {
    allowIntrospection,
    allowedOrigins: allowedOrigins || true,
    allowListPath,
    apiPort: apiPort || 3100,
    cacheEnabled: cacheEnabled || false,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    ALLOW_LIST_PATH,
    API_PORT,
    CACHE_ENABLED,
    GENESIS_FILE_BYRON,
    GENESIS_FILE_SHELLEY,
    HASURA_URI,
    NODE_ENV,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING,
    WHITELIST_PATH
  } = env
  if (WHITELIST_PATH) {
    console.log(`WHITELIST_PATH is deprecated and will be removed in 3.0.0.
    Please update your configuration to use ALLOW_LIST_PATH instead.`)
  }
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true' || NODE_ENV !== 'production',
    allowedOrigins: ALLOWED_ORIGINS,
    allowListPath: ALLOW_LIST_PATH || WHITELIST_PATH,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true',
    genesisFileByron: GENESIS_FILE_BYRON,
    genesisFileShelley: GENESIS_FILE_SHELLEY,
    hasuraUri: HASURA_URI,
    prometheusMetrics: PROMETHEUS_METRICS === 'true',
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true'
  }
}
