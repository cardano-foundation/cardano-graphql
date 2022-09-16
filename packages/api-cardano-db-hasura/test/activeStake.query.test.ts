/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'active_stake'), name)
}

describe('activeStake', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('can return active stake snapshots for an address', async () => {
    const result = await client.query({
      query: await loadQueryNode('activeStakeForAddress'),
      variables: { limit: 5, where: { address: { _eq: 'stake_test1upch4zfr4v2p3y5mp8h6g9r7gmav3s50q86294h3cxl8f6g5rhthq' } } }
    })
    const { activeStake } = result.data
    expect(activeStake.length).toBe(5)
    expect(activeStake[0].amount).toBeDefined()
    expect(activeStake[0].epochNo).toBeDefined()
    expect(activeStake[0].registeredWith.hash).toBeDefined()
    expect(activeStake[0].stakePoolHash).toBeDefined()
    expect(activeStake[0].stakePoolId).toBeDefined()
  })

  it('can return aggregated active stake information for an address', async () => {
    const result = await client.query({
      query: await loadQueryNode('averageActiveStakeForAddress'),
      variables: { address: 'stake_test1up5tgntagnfp04l07waka5p0duv6uw9eujf4axl7jw2wc8gnm6eh0' }
    })
    const { activeStake_aggregate } = result.data
    expect(activeStake_aggregate.aggregate.count).toBeDefined()
    expect(activeStake_aggregate.aggregate.avg.amount).toBeDefined()
  })
})
