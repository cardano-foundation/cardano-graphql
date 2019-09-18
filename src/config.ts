import { Config as ServerConfig } from './Server'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { JormungandrNodeClient } from './lib/JormungandrNodeClient'
import { byronResolvers, jormungandrResolvers, mockedResolvers } from './resolvers'
import { DesktopJormungandrContext, HostedServicesByronContext } from './contexts'

enum NodeImplementation {
  Byron,
  Jormungandr
}

export async function getConfig (): Promise<ServerConfig> {
  const {
    apiPort,
    hasuraUri,
    queryDepthLimit,
    nodeApiUri,
    nodeImplementation,
    tracing
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || 3100,
    context: await buildContext(nodeImplementation, nodeApiUri, hasuraUri),
    queryDepthLimit: queryDepthLimit || 10,
    resolvers: getResolvers(nodeImplementation),
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    HASURA_URI,
    QUERY_DEPTH_LIMIT,
    NODE_API_URI,
    NODE_IMPLEMENTATION,
    TRACING
  } = env
  return {
    apiPort: Number(API_PORT),
    hasuraUri: HASURA_URI,
    nodeApiUri: NODE_API_URI,
    nodeImplementation: NODE_IMPLEMENTATION as NodeImplementation,
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: Boolean(TRACING)
  }
}

async function buildContext (nodeImplementation: NodeImplementation, nodeApiUri?: string, hasuraUri?: string) {
  let context: () => DesktopJormungandrContext | HostedServicesByronContext | void
  if (nodeImplementation === NodeImplementation.Jormungandr && nodeApiUri) {
    context = () => ({
      nodeClient: JormungandrNodeClient({ requestConfig: { baseURL: nodeApiUri }})
    })
  } else if (nodeImplementation === NodeImplementation.Byron && hasuraUri) {
    const hasura = await buildHasuraSchema(hasuraUri)
    context = () => ({ hasura })
  } else {
    context = () => {}
  }
  return context
}

async function getResolvers (nodeImplementation: NodeImplementation) {
  switch (nodeImplementation) {
    case NodeImplementation.Jormungandr :
      return jormungandrResolvers
    case NodeImplementation.Byron :
      return byronResolvers
    default :
      return mockedResolvers
  }
}
