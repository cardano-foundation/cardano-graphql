import { Config as ServerConfig } from './Server'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { hasuraResolvers, mockedResolvers, scalarResolvers } from './resolvers'

export async function getConfig (): Promise<ServerConfig> {
  const {
    apiPort,
    hasuraUri,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || 3100,
    context: hasuraUri ? await buildContext(hasuraUri) : undefined,
    queryDepthLimit: queryDepthLimit || 10,
    resolvers: Object.assign({}, scalarResolvers, hasuraUri ? hasuraResolvers : mockedResolvers),
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    HASURA_URI,
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env
  return {
    apiPort: Number(API_PORT),
    hasuraUri: HASURA_URI,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: Boolean(TRACING)
  }
}

async function buildContext (hasuraUri: string) {
  const hasura = await buildHasuraSchema(hasuraUri)
  return () => ({ hasura })
}
