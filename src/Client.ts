import { fetch } from 'cross-fetch'
import gql from 'graphql-tag'
import { ApolloClient, InMemoryCache, HttpLink } from 'apollo-boost'
import { onError } from 'apollo-link-error'

export interface Config {
  apiUri: string
}

export function Client (config: Config) {
  const httpLink = new HttpLink({
    uri: `${config.apiUri}`,
    fetch
  })

  const errorLink = onError(({ graphQLErrors, networkError }) => {
    if (graphQLErrors) {
      graphQLErrors.map((graphQLError) => {
        throw new Error(graphQLError.message)
      })
    }
    if (networkError) {
      throw new Error(networkError.message)
    }
  })

  const link = errorLink.concat(httpLink)

  const apolloClient = new ApolloClient({
    cache: new InMemoryCache(),
    defaultOptions: {
      query: { fetchPolicy: 'network-only' },
      watchQuery: { fetchPolicy: 'network-only' }
    },
    link
  })

  return {
    apolloClient,
    schema () {
      return apolloClient.query({
        query: gql`query { __schema { types { name } } }`
      })
    }
  }
}
