/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'
import { Genesis } from '@src/graphql_types'

const genesis = {
  byron: require('../../../config/network/mainnet/genesis/byron.json'),
  shelley: require('../../../config/network/mainnet/genesis/shelley.json')
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'tokens'), name)
}

describe('tokens', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('can return information on distinct assets', async () => {
    const result = await client.query({
      query: await loadQueryNode('distinctAssets')
    })
    const { tokens_aggregate } = result.data
    const { aggregate, nodes } = tokens_aggregate
    expect(aggregate.count).toBeDefined()
    if (aggregate.count > 0) {
      expect(nodes[0].assetId).toBeDefined()
      expect(nodes[0].assetName).toBeDefined()
      expect(nodes[0].policyId).toBeDefined()
    }
  })
})
