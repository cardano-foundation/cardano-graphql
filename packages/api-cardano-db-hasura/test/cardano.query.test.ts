import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'cardano'), name)
}

describe('cardano', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090')
  }, 60000)

  it('Returns core information about the current state of the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('chainTipAndCurrentEpochNumber')
    })
    expect(result.data.cardano.tip.number).toBeGreaterThan(3994551)
    expect(result.data.cardano.currentEpoch.number).toBeGreaterThan(184)
  })
})
