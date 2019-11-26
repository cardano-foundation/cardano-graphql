import gql from 'graphql-tag'
import { TestClient } from '../TestClient'

export function cardanoTests (createClient: () => Promise<TestClient>) {
  describe('cardano', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Returns key information about the network', async () => {
      const result = await client.query({
        query: gql`query {
            cardano {
                blockHeight
                currentEpoch {
                    number
                }
                networkName
                protocolConst
                slotDuration
                startTime
            }
        }`
      })
      expect(result.data).toMatchSnapshot()
    })
  })
}
