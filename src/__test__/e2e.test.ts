import { ApolloClient, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import { RetryPromise } from 'promise-exponential-retry'
import fetch from 'cross-fetch'
import * as tests from './tests'

const createClient = async () => {
  const client = new ApolloClient({
    cache: new InMemoryCache({
      addTypename: false
    }),
    link: createHttpLink({
      uri: 'http://localhost:3100',
      fetch
    })
  })
  await RetryPromise.retryPromise(
    'Checking GraphQL server is ready',
    async () => {
      await client.query({
        query: gql`query { cardano { blockHeight }}`
      })
    }, 50)
  return client
}
Object.values(tests).forEach(test => test(createClient))
