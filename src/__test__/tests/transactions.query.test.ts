import BigNumber from 'bignumber.js'
import { tx05ad8b, txe68043 } from '../data_assertions'
import { TestClient } from '../TestClient'
import { loadExampleQueryNode } from '../../util'

export function transactionTests (createClient: () => Promise<TestClient>) {
  describe('transactions', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

    it('Returns transactions by IDs', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'transactionsByIdsOrderByFee'),
        variables: { ids: [txe68043.basic.id, tx05ad8b.basic.id] }
      })
      expect(result.data.transactions.length).toBe(2)
      expect(result.data.transactions[0].inputs[0].index).toBe(0)
      expect(result.data.transactions[0].outputs[0].index).toBe(0)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'aggregateDataWithinTransaction'),
        variables: { ids: [txe68043.aggregated.id, tx05ad8b.aggregated.id] }
      })
      expect(result.data.transactions.length).toBe(2)
      const { transactions: txs } = result.data
      expect(txs).toEqual([tx05ad8b.aggregated, txe68043.aggregated])
      const outputsPlusFee = new BigNumber(txs[1].outputs_aggregate.aggregate.sum.value).plus(txs[1].fee).toString()
      expect(txs[1].inputs_aggregate.aggregate.sum.value).toEqual(outputsPlusFee)
      expect(result.data).toMatchSnapshot()
    })
    it('Can return filtered aggregated data', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'filteredAggregateDataWithinTransaction'),
        variables: { id: txe68043.aggregated_filtered.id }
      })
      expect(result.data).toMatchSnapshot()
    })
  })
}
