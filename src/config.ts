import { MissingConfig, TracingRequired } from './lib/errors'
import { CorsOptions } from 'apollo-server-express'

export type Config = {
  allowedOrigins: CorsOptions['origin']
  apiPort: number
  cacheEnabled: boolean
  hasuraUri: string
  prometheusMetrics: boolean
  queryDepthLimit: number
  tracing: boolean
}

export function getConfig (): Config {
  const {
    allowedOrigins,
    apiPort,
    cacheEnabled,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  if (!hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
  }
  if (prometheusMetrics && process.env.TRACING === 'false') {
    throw new TracingRequired('Prometheus')
  }

  return {
    allowedOrigins: allowedOrigins || true,
    apiPort: apiPort || 3100,
    cacheEnabled: cacheEnabled || false,
    hasuraUri,
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOWED_ORIGINS,
    API_PORT,
    CACHE_ENABLED,
    HASURA_URI,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env
  return {
    allowedOrigins: ALLOWED_ORIGINS,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true',
    hasuraUri: HASURA_URI,
    prometheusMetrics: PROMETHEUS_METRICS === 'true',
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true'
  }
}
