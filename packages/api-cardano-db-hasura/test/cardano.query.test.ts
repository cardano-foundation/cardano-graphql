import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'cardano'), name)
}

describe('cardano', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('Returns core information about the current state of the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('chainTipAndCurrentEpochNumber')
    })
    expect(result.data.cardano.tip.number).toBeGreaterThan(149910)
    expect(result.data.cardano.currentEpoch.number).toBeGreaterThan(30)
  })
})
