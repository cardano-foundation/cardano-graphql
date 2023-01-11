import BigNumber from 'bignumber.js'
import path from 'path'
import { gql } from 'apollo-boost'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'transactions'), name)
}

describe('transactions', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('Returns transactions by hashes', async () => {
    const result = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: {
        hashes: [
          'a3d6f2627a56fe7921eeda546abfe164321881d41549b7f2fbf09ea0b718d758',
          'a00696a0c2d70c381a265a845e43c55e1d00f96b27c06defc015dc92eb206240'
        ]
      }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Returns transactions by hashes with scripts', async () => {
    const plutusResult = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: {
        hashes: [
          '750eed6d314f64d8d5b5fe10a4bc34fe21bbf9c657660e061b26f091dac21717'
        ]
      }
    })
    const timelockResult = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: {
        hashes: [
          'c4f2f4ec91d2afe932c022b9f671cdf44ab62c35e9b6e582335ed01c82d167b0'
        ]
      }
    })
    expect({ plutusResult, timelockResult }).toMatchSnapshot()
  })

  it('Can return ordered by block index', async () => {
    const result = await client.query({
      query: await loadQueryNode('orderedTransactionsInBlock'),
      variables: { blockNumber: 283413 }
    })
    expect(result.data.transactions.length).toBe(456)
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
      variables: { hash: '8f2def6a111c745a92fc93ee8a713974dfcd381d16b47feedef56311d16be90d' }
    })
    expect(result.data.transactions.length).toBe(1)
    expect(result.data.transactions[0].outputs_aggregate.aggregate.count).toBe('0')
    expect(result.data.transactions[0].outputs).toEqual([])
    expect(result.data.transactions[0].totalOutput).toEqual('0')
    expect(result.data.transactions[0].inputs_aggregate.aggregate.count).toBe('1')
  })

  it.skip('Can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinTransaction'),
      variables: {
        hashes: [
          '59f68ea73b95940d443dc516702d5e5deccac2429e4d974f464cc9b26292fd9c'
        ]
      }
    })
    const { transactions: txs } = result.data
    const outputsPlusFee = new BigNumber(txs[0].outputs_aggregate.aggregate.sum.value).plus(txs[0].fee).toString()
    expect(txs[0].inputs_aggregate.aggregate.sum.value).toEqual(outputsPlusFee)
    expect(result.data).toMatchSnapshot()
  })
  it('Can return filtered aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('filteredAggregateDataWithinTransaction'),
      variables: {
        hash: 'a3d6f2627a56fe7921eeda546abfe164321881d41549b7f2fbf09ea0b718d758',
        outputsAddress: 'addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket'
      }
    })
    expect(result.data).toMatchSnapshot()
  })

  describe('metadata', () => {
    it('JSON object', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: '77cb8608db0a84f512e277ba923341775013241401c768ba5214ad2ac004b153' }
      })
      expect(result.data).toMatchSnapshot()
    })

    it('JSON string', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: '23ae1816db236baa5e231166040ae5458b25f34bb33812b2754429f122f85a0d' }
      })
      expect(result.data).toBeDefined()
    })
  })

  describe('transactions with tokens', () => {
    it('shows the tokens minted and output', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithTokens'),
        variables: { hashes: ['c1bb6a765ac42c5bb80e531d1feaa5bd4a4f0d55c331baffa9b014b942995947'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })

  describe('transactions with collateral', () => {
    it('shows the collateral inputs and outputs', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithCollateral'),
        variables: { hashes: ['c1bb6a765ac42c5bb80e531d1feaa5bd4a4f0d55c331baffa9b014b942995947'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })

  describe('transactions with reference inputs', () => {
    it('shows the reference inputs', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithReferenceInputs'),
        variables: { hashes: ['93818dc2e98d924c7379e9a65a44e4890981e675d7e0d23601194c7dff01b0b3'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })
})
