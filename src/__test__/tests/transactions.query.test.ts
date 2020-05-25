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

    it('Returns transactions by hashes', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'transactionsByHashesOrderByFee'),
        variables: { hashes: [txe68043.basic.hash, tx05ad8b.basic.hash] }
      })
      expect(result.data.transactions.length).toBe(2)
      expect(result.data.transactions[0].inputs[0].index).toBe(0)
      expect(result.data.transactions[0].outputs[0].index).toBe(0)
      expect(result.data).toMatchSnapshot()
    })

    it('Can return ordered by block index', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'orderedTransactionsInBlock'),
        variables: { blockNumber: 3979532 }
      })
      expect(result.data.transactions.length).toBe(8)
      expect(result.data.transactions[0].blockIndex).toBe(0)
      expect(result.data.transactions[1].blockIndex).toBe(1)
      expect(result.data.transactions[2].blockIndex).toBe(2)
      expect(result.data.transactions[7].blockIndex).toBe(7)
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: await loadExampleQueryNode('transactions', 'aggregateDataWithinTransaction'),
        variables: { hashes: [txe68043.aggregated.hash, tx05ad8b.aggregated.hash] }
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
        variables: { hash: txe68043.aggregated_filtered.hash }
      })
      expect(result.data).toMatchSnapshot()
    })
  })
}
