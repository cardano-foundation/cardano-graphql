import { TestClient } from '../TestClient'
import { loadExampleQueryNode, loadOperationQueryNode } from '../../util'

export function utxosTests (createClient: () => Promise<TestClient>) {
  describe('utxos', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Can be scoped by address', async () => {
      // Get a addresses to run tests against
      const anyUtxoResult = await client.query({
        query: await loadOperationQueryNode('getAnyUtxoAddress'),
        variables: { qty: 1 }
      })
      const address = anyUtxoResult.data.utxos[0].address
      const result = await client.query({
        query: await loadExampleQueryNode('utxos', 'utxoSetForAddress'),
        variables: { address }
      })
      expect(result.data.utxos[0].transaction.block.number).toBeDefined()
      expect(result.data.utxos.length).toBeDefined()
    })
    it('Can be scoped by list of addresses', async () => {
      // Get a addresses to run tests against
      const anyUtxoResult = await client.query({
        query: await loadOperationQueryNode('getAnyUtxoAddress'),
        variables: { qty: 2 }
      })
      const address1 = anyUtxoResult.data.utxos[0].address
      const address2 = anyUtxoResult.data.utxos[1].address
      const result = await client.query({
        query: await loadExampleQueryNode('utxos', 'utxoSetForAddresses'),
        variables: { addresses: [address1, address2] }
      })
      expect(result.data.utxos.length).toBeDefined()
    })
    it('Can return aggregated UTXO data', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('utxos', 'utxoAggregateValueLessThan'),
        variables: { boundary: '200000' }
      })
      expect(result.data.utxos_aggregate.aggregate.count).toBeDefined()
    })
  })
}
