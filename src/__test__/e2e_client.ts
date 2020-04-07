import { ApolloClient, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import { RetryPromise } from 'promise-exponential-retry'
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
  await RetryPromise.retryPromise(
    'Checking GraphQL server is ready',
    async () => {
      await client.query({
        query: gql`query { 
          cardano { 
            blockHeight
            currentEpoch { 
              number
            }
          }}`
      })
    }, 50)
  return client
}
