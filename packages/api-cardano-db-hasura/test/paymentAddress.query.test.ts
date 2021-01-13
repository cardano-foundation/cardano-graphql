import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'
import { Genesis } from '@src/graphql_types'
import BigNumber from 'bignumber.js'

const genesis = {
  byron: require('../../../config/network/mainnet/genesis/byron.json'),
  shelley: require('../../../config/network/mainnet/genesis/shelley.json')
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'paymentAddress'), name)
}

function loadTestOperationDocument (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, 'graphql_operations'), name)
}

describe('paymentAddress', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('returns payment address summary for the provided addresses', async () => {
    const anyUtxoResult = await client.query({
      query: await loadTestOperationDocument('getAnyUtxoAddress'),
      variables: { qty: 2 }
    })
    const address1 = anyUtxoResult.data.utxos[0].address
    const result = await client.query({
      query: await loadQueryNode('summary'),
      variables: { addresses: [address1] }
    })
    const paymentAddress = result.data.paymentAddresses[0]
    expect(paymentAddress.summary.assetBalances[0].assetName).toEqual('ada')
    expect(new BigNumber(paymentAddress.summary.assetBalances[0].quantity).toNumber())
      .toBeGreaterThan(0)
  })
  it('can bound the summary by chain length by block number', async () => {
    const anyUtxoResult = await client.query({
      query: await loadTestOperationDocument('getAnyUtxoAddress'),
      variables: { qty: 2 }
    })
    const utxo = anyUtxoResult.data.utxos[0]
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
