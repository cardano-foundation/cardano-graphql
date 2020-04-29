import { Config as ServerConfig } from './Server'
import { MissingConfig, TracingRequired } from './lib/errors'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { hasuraResolvers, scalarResolvers } from './resolvers'

export async function getConfig (): Promise<ServerConfig> {
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
    context: hasuraUri ? await buildContext(hasuraUri) : undefined,
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    resolvers: Object.assign({}, scalarResolvers, hasuraResolvers),
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

async function buildContext (hasuraUri: string) {
  const hasura = await buildHasuraSchema(hasuraUri)
  return () => ({ hasura })
}
