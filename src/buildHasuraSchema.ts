import * as path from 'path'
import pRetry, { FailedAttemptError } from 'p-retry'
import { createHttpLink } from 'apollo-link-http'
import { RetryLink } from 'apollo-link-retry'
import { ApolloLink, execute, makePromise } from 'apollo-link'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'
import { loadQueryNode } from './util'

const retries = 3
const onFailedAttemptFor = (message: string) => (error: FailedAttemptError) => {
  console.log(`${message}: Attempt ${error.attemptNumber} of ${retries}, retying...`)
  if (error.retriesLeft === 0) {
    console.error(error)
    process.exit(0)
  }
}

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
    retries,
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
    retries,
    onFailedAttempt: onFailedAttemptFor('Checking Hasura metadata is applied')
  })

  return makeRemoteExecutableSchema({
    schema: await introspectSchema(link),
    link
  })
}
