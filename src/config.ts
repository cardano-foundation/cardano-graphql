import { Config as ServerConfig } from './Server'
import { buildHasuraSchema } from './lib/buildHasuraSchema'

export async function getConfig (): Promise<ServerConfig> {
  const {
    apiPort,
    hasuraUri,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  let context

  if (hasuraUri) {
    const hasura = await buildHasuraSchema(hasuraUri)
    context = () => ({ hasura })
  }

  return {
    apiPort: apiPort || 3100,
    context,
    queryDepthLimit: queryDepthLimit || 10,
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
