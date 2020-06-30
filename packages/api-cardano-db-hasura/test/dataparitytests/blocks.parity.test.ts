import gql from 'graphql-tag'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/index'
import { getDataFromAPI } from './getDataFromApi'

describe('blocks ', () => {
  let client: TestClient
  beforeAll(async () => {
    if (process.env.TEST_MODE === 'e2e') {
      client = await utilDev.createE2EClient()
    } else {
      const schema = await buildSchema('http://localhost:8090')
      client = await utilDev.createIntegrationClient(schema)
    }
  }, 60000)

  it('return the same height', async () => {
    const restResult = await getDataFromAPI('blocks/pages')
    const graphQLResult = await client.query({
      query: gql`query Blockheight {
                  cardano {
                      blockHeight
                  }
              }`
    })

    const restResultBlockHeight = restResult.Right[1][0].cbeBlkHeight
    const graphQLBlockHeight = graphQLResult.data.cardano.blockHeight

    // As we're calling an external API to check equality on something that changes every 20 seconds
    // there is a small delta in the test condition to allow for this where the second API value can be
    // equal to or one more than the first API value.

    expect(graphQLBlockHeight).toBeGreaterThan(restResultBlockHeight - 1)
    expect(graphQLBlockHeight).toBeLessThanOrEqual(restResultBlockHeight + 1)
  })
})
