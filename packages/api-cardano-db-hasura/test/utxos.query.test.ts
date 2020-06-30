import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/index'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'utxos'), name)
}
describe('utxos', () => {
  let client: TestClient
  beforeAll(async () => {
    if (process.env.TEST_MODE === 'e2e') {
      client = await utilDev.createE2EClient()
    } else {
      const schema = await buildSchema('http://localhost:8090')
      client = await utilDev.createIntegrationClient(schema)
    }
  }, 60000)

  it('Can be scoped by address', async () => {
    const result = await client.query({
      query: await loadQueryNode('utxoSetForAddress'),
      variables: { address: 'DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s' }
    })
    expect(result.data.utxos.length).toBeDefined()
  })
  it('Can be scoped by list of addresses', async () => {
    const result = await client.query({
      query: await loadQueryNode('utxoSetForAddresses'),
      variables: {
        addresses: [
          'DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s',
          'Ae2tdPwUPEZGvXJ3ebp4LDgBhbxekAH2oKZgfahKq896fehv8oCJxmGJgLt'
        ]
      }
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
