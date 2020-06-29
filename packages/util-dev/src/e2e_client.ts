import { ApolloClient, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import pRetry from 'p-retry'
import fetch from 'cross-fetch'
import util from '@cardano-graphql/util'

export const createE2EClient = async () => {
  const client = new ApolloClient({
    cache: new InMemoryCache({
      addTypename: false
    }),
    link: createHttpLink({
      uri: process.env.CARDANO_GRAPHQL_URI || 'http://localhost:3100',
      fetch
    })
  })
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
    retries: 9,
    onFailedAttempt: util.onFailedAttemptFor('Cardano GraphQL Server readiness')
  })
  return client
}
