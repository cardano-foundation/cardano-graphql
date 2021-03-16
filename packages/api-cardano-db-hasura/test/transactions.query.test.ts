import BigNumber from 'bignumber.js'
import path from 'path'
import { gql } from 'apollo-boost'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { tx05ad8b, txe68043, tx0b8c5b } from './data_assertions'
import { buildClient } from './util'
import { Genesis } from '@src/graphql_types'

const genesis = {
  byron: require('../../../config/network/mainnet/genesis/byron.json'),
  shelley: require('../../../config/network/mainnet/genesis/shelley.json')
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'transactions'), name)
}

describe('transactions', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('Returns transactions by hashes', async () => {
    const result = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: {
        hashes: [
          txe68043.basic.hash,
          tx0b8c5b.basic.hash,
          tx05ad8b.basic.hash
        ]
      }
    })
    expect(result.data.transactions.length).toBe(3)
    expect(result.data.transactions[0].inputs[0].sourceTxHash).toBe(txe68043.basic.inputs[0].sourceTxHash)
    expect(result.data.transactions[0].outputs[0].index).toBe(0)
    expect(result.data.transactions[2].inputs[0].sourceTxHash).toBe(tx05ad8b.basic.inputs[0].sourceTxHash)

    const txWithWithdrawals = {
      fee: result.data.transactions[1].fee,
      totalOutput: result.data.transactions[1].totalOutput,
      inputsTotal: new BigNumber(result.data.transactions[1].inputs_aggregate.aggregate.sum.value),
      totalWithdrawals: new BigNumber(result.data.transactions[1].withdrawals_aggregate.aggregate.sum.amount)
    }
    const combinedInputs = txWithWithdrawals.inputsTotal.plus(txWithWithdrawals.totalWithdrawals)
    expect(txWithWithdrawals.totalOutput).toBe(combinedInputs.minus(txWithWithdrawals.fee).toString())
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

  it('returns an empty array when the transactions has no outputs', async () => {
    const result = await client.query({
      query: gql`query transactionWithNoOutputs(
          $hash: Hash32Hex!
      ) {
          transactions(
              where: { hash: { _eq: $hash } },
          ) {
              outputs {
                  address
                  value
              }
              outputs_aggregate {
                  aggregate {
                      count
                  }
              }
              inputs_aggregate {
                  aggregate {
                      count
                  }
              }
              totalOutput
          }
      }`,
      variables: { hash: 'b4caa8ed7bbcffb945bfcb7e61bce574cc15822d06c5ac0a74694b232361d09b' }
    })
    expect(result.data.transactions.length).toBe(1)
    expect(result.data.transactions[0].outputs_aggregate.aggregate.count).toBe('0')
    expect(result.data.transactions[0].outputs).toEqual([])
    expect(result.data.transactions[0].totalOutput).toEqual('0')
    expect(result.data.transactions[0].inputs_aggregate.aggregate.count).toBe('1')
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

  describe('metadata', () => {
    it('JSON object', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: 'f910021138e553c65b96cf3e4647927fcd9f634e06544251f83cffb1891876e8' }
      })
      expect(result.data).toMatchSnapshot()
    })

    it('JSON string', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: '204ca3088bbab666692f39dddb9b773e6fb20b0d0c3e464407985fa7863e5bac' }
      })
      expect(result.data).toMatchSnapshot()
    })
  })


  describe('transactions with tokens', () => {
    it('shows the tokens minted and output', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithTokens'),
        variables: { hashes: ['e252be4c7e40d35919f741c9649ff207c3e49d53bb819e5c1cb458055fd363ed'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })
})
