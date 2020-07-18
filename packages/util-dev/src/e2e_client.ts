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
    defaultOptions: {
      query: {
        fetchPolicy: 'network-only'
      }
    },
    link: createHttpLink({
      uri: process.env.CARDANO_GRAPHQL_URI || 'http://localhost:3100',
      fetch
    })
  })
  await pRetry(async () => {
    const result = await client.query({
      query: gql`query {
          cardanoDbMeta {
              initialized
              syncPercentage
          }}`
    })
    if (result.data?.cardanoDbMeta.initialized === false) {
      throw new Error(`Cardano DB is not initialized: ${JSON.stringify(result.data)}`)
    }
  }, {
    factor: 1.75,
    retries: 9,
    onFailedAttempt: util.onFailedAttemptFor('Cardano GraphQL Server readiness')
  })
  return client
}
