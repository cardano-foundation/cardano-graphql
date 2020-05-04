import * as path from 'path'
import { RetryPromise } from 'promise-exponential-retry'
import { createHttpLink } from 'apollo-link-http'
import { RetryLink } from 'apollo-link-retry'
import { ApolloLink, execute, makePromise } from 'apollo-link'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'
import { loadQueryNode } from './util'

export async function buildHasuraSchema (hasuraUri: string) {
  const httpLink = createHttpLink({
    uri: hasuraUri,
    fetch,
    headers: { 'X-Hasura-Role': 'cardano-graphql' }
  })

  const link = ApolloLink.from([
    new RetryLink(),
    httpLink
  ])

  await RetryPromise.retryPromise('Hasura Schema introspection', async () => {
    const schema = await introspectSchema(link)
    const coreTypes = ['Cardano', 'Epoch', 'Slot', 'Block', 'Transaction']
    coreTypes.forEach(t => {
      const gqlType = schema.getType('Block')
      if (!gqlType) {
        throw new Error(`Remote schema is missing ${t}`)
      }
    })
  }, 30)

  // Hasura applies metadata after the server is booted, so there is a potential race condition
  // when provisioning a new deployment we need to protect against.
  await RetryPromise.retryPromise('Checking Hasura metadata is applied', async () => {
    const result = await makePromise(execute(link, {
      query: await loadQueryNode(path.resolve(__dirname, 'graphql_operations', 'hasuraStateCheck.graphql'))
    }))
    if (!result.data || result.data.blocks[0].transactions_aggregate.aggregate.count === null) {
      throw new Error('Hasura Metadata not applied')
    }
  }, 30)

  return makeRemoteExecutableSchema({
    schema: await introspectSchema(link),
    link
  })
}
