import { ApolloClient, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'

export const createE2EClient = async (uri?: string) => {
  return new ApolloClient({
    cache: new InMemoryCache({
      addTypename: false
    }),
    defaultOptions: {
      query: {
        fetchPolicy: 'network-only'
      }
    },
    link: createHttpLink({
      uri: uri || process.env.CARDANO_GRAPHQL_URI || 'http://localhost:3100',
      fetch
    })
  })
}
