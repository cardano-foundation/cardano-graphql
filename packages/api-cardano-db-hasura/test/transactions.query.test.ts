import BigNumber from 'bignumber.js'
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { tx05ad8b, txe68043 } from './data_assertions'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'transactions'), name)
}

describe('transactions', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient()
  }, 60000)

  it('Returns transactions by hashes', async () => {
    const result = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: { hashes: [txe68043.basic.hash, tx05ad8b.basic.hash] }
    })
    expect(result.data.transactions.length).toBe(2)
    expect(result.data.transactions[0].inputs[0].sourceTxHash).toBe(txe68043.basic.sourceTxHash)
    expect(result.data.transactions[1].inputs[0].sourceTxHash).toBe(tx05ad8b.basic.sourceTxHash)
    expect(result.data.transactions[0].outputs[0].index).toBe(0)
    expect(result.data).toMatchSnapshot()
  })

  it('Can return ordered by block index', async () => {
    const result = await client.query({
      query: await loadQueryNode('orderedTransactionsInBlock'),
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
      query: await loadQueryNode('aggregateDataWithinTransaction'),
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
      query: await loadQueryNode('filteredAggregateDataWithinTransaction'),
      variables: { hash: txe68043.aggregated_filtered.hash }
    })
    expect(result.data).toMatchSnapshot()
  })
})
