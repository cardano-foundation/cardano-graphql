import gql from 'graphql-tag'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { testApolloServer } from './support/testApolloServer'

describe('utxoSet', () => {
  let client: ApolloServerTestClient

  beforeEach(async () => {
    client = createTestClient(await testApolloServer())
  }, 60000)

  it('Can be scoped by address', async () => {
    const result = await client.query({
      query: gql`query {
          utxoSet(
              order_by: { address: asc }
              where: { address: { _eq:
              "DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s"
              }
              }
          ) {
              address
              value
          }
      }`
    })
    expect(result.data.utxoSet.length).toBe(2)
    expect(result).toMatchSnapshot()
  })
  it('Can be scoped by list of addresses', async () => {
    const result = await client.query({
      query: gql`query {
          utxoSet(
              order_by: { address: asc }
              where: { address: { _in: [
                  "DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s",
                  "Ae2tdPwUPEZGvXJ3ebp4LDgBhbxekAH2oKZgfahKq896fehv8oCJxmGJgLt"
              ]}}
          ) {
              address
              value
          }
      }`
    })
    expect(result.data.utxoSet.length).toBe(3)
    expect(result).toMatchSnapshot()
  })
})
