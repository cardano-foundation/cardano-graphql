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
    client = await buildClient()
  }, 60000)

  it('Returns static information about the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('static')
    })
    expect(result.data).toMatchSnapshot()
  })
  it('Returns dynamic information about the network', async () => {
    const result = await client.query({
      query: await loadQueryNode('dynamic')
    })
    expect(result.data.cardano.tip.number).toBeGreaterThan(3994551)
    expect(result.data.cardano.currentEpoch.number).toBeGreaterThan(184)
  })
})
