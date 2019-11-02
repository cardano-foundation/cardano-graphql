import { ApolloClient, InMemoryCache } from 'apollo-boost'
import { ApolloServerTestClient } from 'apollo-server-testing'

export type TestClient = ApolloClient<InMemoryCache> | ApolloServerTestClient
