import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import BigNumber from 'bignumber.js'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'paymentAddress'), name)
}

function loadTestOperationDocument (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, 'graphql_operations'), name)
}

describe('paymentAddress', () => {
  let client: TestClient
  let utxos: { address: string, transaction: { block: { number: number } } }[]

  beforeAll(async () => {
    client = await testClient.preprod()
    const result = await client.query({
      query: await loadTestOperationDocument('getAnyUtxoWithBlock'),
      variables: { qty: 2 }
    })
    utxos = result.data.utxos
  })

  it('returns payment address summary for the provided addresses', async () => {
    const result = await client.query({
      query: await loadQueryNode('summary'),
      variables: { addresses: [utxos[0].address] }
    })
    const paymentAddress = result.data.paymentAddresses[0]
    expect(paymentAddress.summary.assetBalances[0].asset.assetId).toBeDefined()
    expect(new BigNumber(paymentAddress.summary.assetBalances[0].quantity).toNumber())
      .toBeGreaterThan(0)
  })

  it('returns summaries for multiple addresses in one query', async () => {
    const addresses = utxos.map((u) => u.address)
    const result = await client.query({
      query: await loadQueryNode('summary'),
      variables: { addresses }
    })
    expect(result.data.paymentAddresses.length).toBe(addresses.length)
    result.data.paymentAddresses.forEach((pa: { summary: { assetBalances: unknown[] } }) => {
      expect(pa.summary.assetBalances.length).toBeGreaterThan(0)
    })
  })

  it('can bound the summary by chain length by block number', async () => {
    const utxo = utxos[0]
    const blockBound = utxo.transaction.block.number - 1
    const unboundedResult = await client.query({
      query: await loadQueryNode('summary'),
      variables: { addresses: [utxo.address] }
    })
    const boundedResult = await client.query({
      query: await loadQueryNode('summary'),
      variables: {
        addresses: [utxo.address],
        atBlock: blockBound
      }
    })
    const unboundedAdaBalance = new BigNumber(
      unboundedResult.data.paymentAddresses[0].summary.assetBalances[0].quantity
    ).toNumber()
    const boundedAdaBalance = new BigNumber(
      boundedResult.data.paymentAddresses[0].summary?.assetBalances[0]?.quantity
    ).toNumber() || 0
    expect(unboundedAdaBalance).toBeGreaterThan(boundedAdaBalance)
  })
})
