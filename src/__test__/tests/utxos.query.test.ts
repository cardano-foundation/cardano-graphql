import { TestClient } from '../TestClient'
import { loadQueryNode } from '../../util'

export function utxosTests (createClient: () => Promise<TestClient>) {
  describe('utxos', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Can be scoped by address', async () => {
      const result = await client.query({
        query: await loadQueryNode('utxos', 'utxoSetForAddress'),
        variables: { address: 'DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s' }
      })
      expect(result.data.utxos.length).toBeDefined()
    })
    it('Can be scoped by list of addresses', async () => {
      const result = await client.query({
        query: await loadQueryNode('utxos', 'utxoSetForAddresses'),
        variables: { addresses: [
          'DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s',
          'Ae2tdPwUPEZGvXJ3ebp4LDgBhbxekAH2oKZgfahKq896fehv8oCJxmGJgLt'
        ] }
      })
      expect(result.data.utxos.length).toBeDefined()
    })
    it('Can return aggregated UTXO data', async () => {
      const result = await client.query({
        query: await loadQueryNode('utxos', 'utxoAggregateValueLessThan'),
        variables: { boundary: '200000' }
      })
      expect(result.data.utxos_aggregate.aggregate.count).toBeDefined()
    })
  })
}
