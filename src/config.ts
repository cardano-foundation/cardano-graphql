import { Config as ServerConfig } from './Server'
import { MissingConfig } from './lib/errors'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { hasuraResolvers, scalarResolvers } from './resolvers'

export async function getConfig (): Promise<ServerConfig> {
  const {
    allowedOrigins,
    apiPort,
    cacheEnabled,
    hasuraUri,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  if (!hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
  }

  return {
    allowedOrigins: allowedOrigins || true,
    apiPort: apiPort || 3100,
    cacheEnabled: cacheEnabled || false,
    context: hasuraUri ? await buildContext(hasuraUri) : undefined,
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
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env
  return {
    allowedOrigins: ALLOWED_ORIGINS,
    apiPort: Number(API_PORT),
    cacheEnabled: Boolean(CACHE_ENABLED),
    hasuraUri: HASURA_URI,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: Boolean(TRACING)
  }
}

async function buildContext (hasuraUri: string) {
  const hasura = await buildHasuraSchema(hasuraUri)
  return () => ({ hasura })
}
