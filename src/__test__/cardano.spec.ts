import gql from 'graphql-tag'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { testApolloServer } from './support/testApolloServer'

describe('cardano', () => {
  let client: ApolloServerTestClient

  beforeEach(async () => {
    client = createTestClient(await testApolloServer())
  }, 60000)

  it('Returns key information about the network', async () => {
    const result = await client.query({
      query: gql`query {
          cardano {
              blockHeight
              currentEpoch {
                  number
              }
              protocolConst
              slotDuration
              startTime
          }
      }`
    })
    expect(result).toMatchSnapshot()
  })

})
