import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'redeemers'), name)
}

describe('redeemers', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.mainnet()
  })

  it('can return an array of redeemers', async () => {
    const result = await client.query({
      query: await loadQueryNode('redeemers'),
      variables: {
        limit: 1
      }
    })
    expect(result.data.redeemers[0].fee).not.toBeNull()
    expect(result.data.redeemers[0].index).not.toBeNull()
    expect(result.data.redeemers[0].purpose).not.toBeNull()
    expect(result.data.redeemers[0].scriptHash).not.toBeNull()
    expect(result.data.redeemers[0].transaction).not.toBeNull()
    expect(result.data.redeemers[0].unitMem).not.toBeNull()
    expect(result.data.redeemers[0].unitSteps).not.toBeNull()
  })

  it('can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('redeemersAggregate')
    })
    expect(result.data.redeemers_aggregate.aggregate.avg.value).not.toBeNull()
    expect(result.data.redeemers_aggregate.aggregate.max.value).not.toBeNull()
    expect(result.data.redeemers_aggregate.aggregate.min.value).not.toBeNull()
    expect(result.data.redeemers_aggregate.aggregate.sum.value).not.toBeNull()
  })
})
