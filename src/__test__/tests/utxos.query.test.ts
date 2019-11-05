import gql from 'graphql-tag'
import { TestClient } from '../TestClient'

export function utxosTests (createClient: () => Promise<TestClient>) {
  describe('utxos', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Can be scoped by address', async () => {
      const result = await client.query({
        query: gql`query {
            utxos(
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
      expect(result.data.utxos.length).toBe(2)
      expect(result.data).toMatchSnapshot()
    })
    it('Can be scoped by list of addresses', async () => {
      const result = await client.query({
        query: gql`query {
            utxos(
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
      expect(result.data.utxos.length).toBe(3)
      expect(result.data).toMatchSnapshot()
    })
    it('Can return aggregated UTXO data', async () => {
      const result = await client.query({
        query: gql`query {
            utxos_aggregate( where: { value: { _lt: "200000" }}) {
                aggregate {
                    avg {
                        value
                    }
                    count
                    max {
                        value
                    }
                    min {
                        value
                    }
                    sum {
                        value
                    }
                }
            }
        }`
      })
      expect(result.data.utxos_aggregate.aggregate.count).toBe(68)
      expect(result.data).toMatchSnapshot()
    })
  })
}
