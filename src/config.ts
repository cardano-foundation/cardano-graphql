import { Config as ServerConfig } from './Server'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { jormungandrResolvers, mockedResolvers } from './resolvers'

enum NodeImplementation {
  Jormungandr
}

export async function getConfig (): Promise<ServerConfig> {
  const {
    apiPort,
    nodeImplementation,
    hasuraUri,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  const hasura = hasuraUri ? await buildHasuraSchema(hasuraUri) : undefined
  const context = hasuraUri ? () => ({ hasura }) : undefined

  return {
    apiPort: apiPort || 3100,
    context,
    queryDepthLimit: queryDepthLimit || 10,
    resolvers: (nodeImplementation === NodeImplementation.Jormungandr) ? jormungandrResolvers : mockedResolvers,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    HASURA_URI,
    NODE_IMPLEMENTATION,
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env
  return {
    apiPort: Number(API_PORT),
    hasuraUri: HASURA_URI,
    nodeImplementation: NODE_IMPLEMENTATION as NodeImplementation,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: Boolean(TRACING)
  }
}
