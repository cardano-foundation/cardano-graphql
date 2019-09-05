import { RetryPromise } from 'promise-exponential-retry'
import { createHttpLink } from 'apollo-link-http'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'
import { Config as ServerConfig } from '../Server'

export async function buildHasuraSchema (hasuraUri: ServerConfig['hasuraUri']) {
  const link = createHttpLink({
    uri: hasuraUri,
    fetch
  })
  return makeRemoteExecutableSchema({
    schema: await RetryPromise.retryPromise(
      'Connecting to Hasura',
      () => introspectSchema(link),
      30
    ),
    link
  })
}
