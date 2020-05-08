import { ApolloClient, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import pRetry, { FailedAttemptError } from 'p-retry'
import fetch from 'cross-fetch'

export const createClient = async () => {
  const client = new ApolloClient({
    cache: new InMemoryCache({
      addTypename: false
    }),
    link: createHttpLink({
      uri: process.env.CARDANO_GRAPHQL_URI || 'http://localhost:3100',
      fetch
    })
  })
  const retries = 10
  await pRetry(async () => {
    await client.query({
      query: gql`query {
          cardano {
              blockHeight
              currentEpoch {
                  number
              }
          }}`
    })
  }, {
    retries,
    onFailedAttempt: (error: FailedAttemptError) => {
      console.log(`Hasura schema introspection: Attempt ${error.attemptNumber} of ${retries}, retying...`)
      if (error.retriesLeft === 0) {
        console.error(error)
        process.exit(0)
      }
    }
  })
  return client
}
