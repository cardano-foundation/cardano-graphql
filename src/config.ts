import { Config as ServerConfig } from './Server'

export function getConfig (): ServerConfig {
  const {
    apiPort,
    hasuraUri,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || 3100,
    hasuraUri: hasuraUri || 'http://localhost:8090/v1/graphql',
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
