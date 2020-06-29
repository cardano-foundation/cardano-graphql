import { ApolloServerBase } from 'apollo-server-core'
import { createTestClient } from 'apollo-server-testing'
import { GraphQLSchema } from 'graphql'
import { TestClient } from './TestClient'

/**
 * A client with an embedded server compatible with apollo-server-testing
 *
 * Enables direct querying of the service, sans HTTP server.
 *
 * @packageDocumentation
 */

export function createIntegrationClient (schema: GraphQLSchema): TestClient {
  return createTestClient(new ApolloServerBase({
    schema
  }))
}
