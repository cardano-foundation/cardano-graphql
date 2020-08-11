import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'utxos'), name)
}

function loadTestOperationDocument (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, 'graphql_operations'), name)
}

describe('utxos', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442)
  }, 60000)

  it('Can be scoped by address', async () => {
    // Get a addresses to run tests against
    const anyUtxoResult = await client.query({
      query: await loadTestOperationDocument('getAnyUtxoAddress'),
      variables: { qty: 1 }
    })
    const address = anyUtxoResult.data.utxos[0].address
    const result = await client.query({
      query: await loadQueryNode('utxoSetForAddress'),
      variables: { address }
    })
    expect(result.data.utxos[0].transaction.block.number).toBeDefined()
    expect(result.data.utxos.length).toBeDefined()
  })
  it('Can be scoped by list of addresses', async () => {
    // Get a addresses to run tests against
    const anyUtxoResult = await client.query({
      query: await loadTestOperationDocument('getAnyUtxoAddress'),
      variables: { qty: 2 }
    })
    const address1 = anyUtxoResult.data.utxos[0].address
    const address2 = anyUtxoResult.data.utxos[1].address
    const result = await client.query({
      query: await loadQueryNode('utxoSetForAddresses'),
      variables: { addresses: [address1, address2] }
    })
    expect(result.data.utxos.length).toBeDefined()
  })
  it('Can return aggregated UTXO data', async () => {
    const result = await client.query({
      query: await loadQueryNode('utxoAggregateValueLessThan'),
      variables: { boundary: '200000' }
    })
    expect(result.data.utxos_aggregate.aggregate.count).toBeDefined()
  })
})
