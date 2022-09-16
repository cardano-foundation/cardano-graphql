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
      variables: { blockNumber: 24366 }
    })
    expect(result.data.transactions.length).toBe(267)
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
      variables: { hash: '7adb3c5dc48d2c5e27a1b8809a05a2fce432d795ec692fa164d1e5aa398f22bd' }
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
          '327196faecffd59550d506c1a452b09667ea4a85644d26ca25759e33e3804e76'
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
        outputsAddress: 'addr_test1vpeapc7wf8tep752lvaaxd8e9dfl2cs2sgpc72nwtekm8xspzgj0u'
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

  describe('transactions with collateral', () => {
    it('shows the collateral inputs and outputs', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithCollateral'),
        variables: { hashes: ['3fca8f8be6efba69327388dac1d0b37a72f8803ada94f724aa8bbf17b7e38eee'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })

  describe('transactions with reference inputs', () => {
    it('shows the reference inputs', async () => {
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithReferenceInputs'),
        variables: { hashes: ['ceaada570dd66d575cb813caa5146fa3c86c9aa88e4b23b7087cd44b70c65bc5'] }
      })
      expect(result.data).toMatchSnapshot()
    })
  })

  // TODO: not finishes correctly
  // describe('transactions with invalid inputs', () => {
  //   it('can recover from ogmios disconnect after invalid tx', async () => {
  //     // submit tx that triggers ogmios disconnect with error 1006
  //     client.mutate({
  //       mutation: await loadQueryNode('submitTransaction'),
  //       variables: { transaction: '83a40081825820bebc3522ae5de98b371e4f9ef6349456558aabf63acc8a23fc9e9d78d82e18a300018382583282d818582883581cc4b20dbcc149a58d0d56194d94f5405d31d2b03a223c4a7c0cd62f11a102451a4170cb17001aa8dcbbd21a01ba814082583282d818582883581c98465c3fe4b23992f94a9fdf6d290481bb02fffc6dd08a2ccf1b5eb6a102451a4170cb17001af8dc48ca1b000000012926ef8382583282d818582883581c98465c3fe4b23992f94a9fdf6d290481bb02fffc6dd08a2ccf1b5eb6a102451a4170cb17001af8dc48ca1b000000012926ef82021a000383bb031a0069db33a10281845820078061be0ed1b55a9e18711c24c678288eb0d01544325a4edd24278ff72d0e45584077c1d0c0ad8838377c69f0c17a8739375dfa740c64c0d00d553225ac71d1e30f821cd8cef7ddd2d367e2c744b027b6f7d80653a1b6e4dfaf05ddc89b5b5114045820b6ed5cad26f7d7da7b0ebcfe5955fc4df995a330b2be52f9dcb3687a0c78d4d448a102451a4170cbabcd' }
  //     })
  //     // check that client successfully reconnected
  //     const result = await client.query({
  //       query: await loadQueryNode('transactionsByHashesWithReferenceInputs'),
  //       variables: { hashes: ['067e84ddc353faefcbd29f11586d854146b3df43f89b3c1a64a3ff9fbb3c05bd'] }
  //     })
  //     expect(result.data.transactions.length).toBeGreaterThan(0)
  //   })
  // })
})
