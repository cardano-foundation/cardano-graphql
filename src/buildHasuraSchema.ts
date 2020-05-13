import * as path from 'path'
import pRetry from 'p-retry'
import { createHttpLink } from 'apollo-link-http'
import { RetryLink } from 'apollo-link-retry'
import { ApolloLink, execute, makePromise } from 'apollo-link'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'
import { loadQueryNode, onFailedAttemptFor } from './util'

export async function buildHasuraSchema (hasuraUri: string) {
  const httpLink = createHttpLink({
    uri: `${hasuraUri}/v1/graphql`,
    fetch,
    headers: { 'X-Hasura-Role': 'cardano-graphql' }
  })

  const link = ApolloLink.from([
    new RetryLink(),
    httpLink
  ])

  await pRetry(async () => {
    const schema = await introspectSchema(link)
    const coreTypes = ['Cardano', 'Epoch', 'Block', 'Transaction']
    coreTypes.forEach(t => {
      const gqlType = schema.getType(t)
      if (!gqlType) {
        throw new Error(`Remote schema is missing ${t}`)
      }
    })
  }, {
    retries: 9,
    onFailedAttempt: onFailedAttemptFor('Hasura schema introspection')
  })

  // Hasura applies metadata after the server is booted, so there is a potential race condition
  await pRetry(async () => {
    const result = await makePromise(execute(link, {
      query: await loadQueryNode(path.resolve(__dirname, 'graphql_operations', 'hasuraStateCheck.graphql'))
    }))
    if (!result.data || result.data.blocks[0].transactions_aggregate.aggregate.count === null) {
      throw new Error('Hasura Metadata not applied')
    }
  }, {
    retries: 9,
    onFailedAttempt: onFailedAttemptFor('Checking Hasura metadata is applied')
  })

  return makeRemoteExecutableSchema({
    schema: await introspectSchema(link),
    link
  })
}
