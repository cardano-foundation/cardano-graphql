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
          'd13e153c6df662e565b4d5608d4224de21fb387c5371f499d788e82912069324',
          '657ac5fa926f9f047360cbc908a4f6177f2ac9b52d4254c63dcb1312127217f4'
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
          '39f065894a51ddf75f58f577e648de5990d9eddf239619e969ebb8aa3a0ea551'
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
      variables: { blockNumber: 3037760 }
    })
    expect(result.data.transactions.length).toBe(22)
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
      variables: { hash: '0576d6e3fabccabe120f19368bdc0b5a181759f845704e7b5eb01a2bfe948610' }
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
      variables: {
        hashes: [
          'b5aff847f8cefe95f3ae06ae5850d19d33301a5aa3aedc618a3729558403e3db'
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
        hash: '2819f6a34f9029251eb91309c85d4b42d7d8dd26ed00ea0693463176bac62c45',
        inputsValueGt: '3842014',
        outputsAddress: 'addr_test1vzhv4acdxm6v00m2h62pllw4l6qtymh3pwkpr9urptp8zxccuawfr'
      }
    })
    expect(result.data).toMatchSnapshot()
  })

  describe('metadata', () => {
    it('JSON object', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: '45fff8715b952f04cd84f6235b1a6f22d9437c2d60a699317f08bba60e2965a6' }
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
        variables: { hashes: ['fad76833b223b1ba778c4de016c6224cb56f29f48d286ee84090aa80afec1a58'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })
})
