import { CorsOptions } from 'apollo-server-express'
import { IntrospectionNotPermitted, MissingConfig, TracingRequired } from './errors'

export type Config = {
  allowIntrospection?: boolean
  allowedOrigins: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  genesisFileByron: string
  genesisFileShelley: string
  hasuraUri: string
  poolMetadataProxy: string
  prometheusMetrics: boolean
  queryDepthLimit: number
  tracing: boolean
  whitelistPath: string
}

export async function getConfig (): Promise<Config> {
  const {
    allowIntrospection,
    allowedOrigins,
    apiPort,
    cacheEnabled,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    poolMetadataProxy,
    prometheusMetrics,
    queryDepthLimit,
    tracing,
    whitelistPath
  } = filterAndTypecastEnvs(process.env)

  if (!hasuraUri && !genesisFileShelley) {
    throw new MissingConfig('You have not provided configuration to load an API segment. Either set HASURA_URI or GENESIS_FILE_SHELLEY')
  }
  if (prometheusMetrics && process.env.TRACING === 'false') {
    throw new TracingRequired('Prometheus')
  }
  if (whitelistPath && allowIntrospection) {
    throw new IntrospectionNotPermitted('whitelist')
  }
  return {
    allowIntrospection,
    allowedOrigins: allowedOrigins || true,
    apiPort: apiPort || 3100,
    cacheEnabled: cacheEnabled || false,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    poolMetadataProxy,
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    tracing,
    whitelistPath
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    API_PORT,
    CACHE_ENABLED,
    GENESIS_FILE_BYRON,
    GENESIS_FILE_SHELLEY,
    HASURA_URI,
    POOL_METADATA_PROXY,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING,
    WHITELIST_PATH
  } = env
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true' ? true : undefined,
    allowedOrigins: ALLOWED_ORIGINS,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true' ? true : undefined,
    genesisFileByron: GENESIS_FILE_BYRON,
    genesisFileShelley: GENESIS_FILE_SHELLEY,
    hasuraUri: HASURA_URI,
    poolMetadataProxy: POOL_METADATA_PROXY,
    prometheusMetrics: PROMETHEUS_METRICS === 'true' ? true : undefined,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true' ? true : undefined,
    whitelistPath: WHITELIST_PATH
  }
}
