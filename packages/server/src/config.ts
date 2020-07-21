import { CorsOptions } from 'apollo-server-express'
import { IntrospectionNotPermitted, MissingConfig, TracingRequired } from './errors'

export type Config = {
  allowIntrospection?: boolean
  allowedOrigins: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  cardanoNodeSocketPath: string
  genesisFile: string
  hasuraUri: string
  prometheusMetrics: boolean
  queryDepthLimit: number
  tracing: boolean
  whitelistPath: string
  testnet: string
  cardanoCli: string
}

export async function getConfig (): Promise<Config> {
  const {
    allowIntrospection,
    allowedOrigins,
    apiPort,
    cacheEnabled,
    cardanoNodeSocketPath,
    genesisFile,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit,
    tracing,
    whitelistPath,
    testnet,
    cardanoCli
  } = filterAndTypecastEnvs(process.env)
  if (!cardanoNodeSocketPath) {
    throw new MissingConfig('CARDANO_NODE_SOCKET_PATH env not set')
  }
  if (!genesisFile) {
    throw new MissingConfig('GENESIS_FILE env not set')
  }
  if (!hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
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
    cardanoNodeSocketPath,
    genesisFile,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    tracing,
    whitelistPath,
    testnet,
    cardanoCli
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    API_PORT,
    CACHE_ENABLED,
    CARDANO_NODE_SOCKET_PATH,
    GENESIS_FILE,
    HASURA_URI,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING,
    WHITELIST_PATH,
    CARDANO_CLI_CMD,
    CARDANO_MAGIC
  } = env
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true' ? true : undefined,
    allowedOrigins: ALLOWED_ORIGINS,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true' ? true : undefined,
    cardanoNodeSocketPath: CARDANO_NODE_SOCKET_PATH,
    genesisFile: GENESIS_FILE,
    hasuraUri: HASURA_URI,
    prometheusMetrics: PROMETHEUS_METRICS === 'true' ? true : undefined,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true' ? true : undefined,
    whitelistPath: WHITELIST_PATH,
    testnet: CARDANO_MAGIC ? `--testnet-magic ${CARDANO_MAGIC}` : '--mainnet',
    cardanoCli: CARDANO_CLI_CMD || 'cardano-cli'
  }
}
