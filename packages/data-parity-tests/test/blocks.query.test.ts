import gql from 'graphql-tag'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { getDataFromAPI } from '@src/util'

describe('blocks ', () => {
  let client: TestClient
  beforeAll(async () => {
    process.env.CARDANO_GRAPHQL_URI = '3201'
    client = await utilDev.createE2EClient()
  }, 60000)

  it('return the same height', async () => {
    const restResult = await getDataFromAPI('blocks/pages')
    const graphQLResult = await client.query({
      query: gql`query Blockheight {
                  cardano {
                      tip { number }
                  }
              }`
    })

    const restResultBlockHeight = restResult.Right[1][0].cbeBlkHeight
    const graphQLBlockHeight = graphQLResult.data.cardano.tip.number

    // As we're calling to check equality on something that changes every 20 seconds
    // there is a small delta in the test condition to allow for this where the second API value can be
    // equal to or one more than the first API value.

    expect(graphQLBlockHeight).toBeGreaterThan(restResultBlockHeight - 1)
    expect(graphQLBlockHeight).toBeLessThanOrEqual(restResultBlockHeight + 1)
  })
})
