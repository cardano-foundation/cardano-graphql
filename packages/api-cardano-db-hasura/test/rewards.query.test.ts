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
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'rewards'), name)
}

describe('rewards', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('can return details for rewards scoped to an address', async () => {
    const result = await client.query({
      query: await loadQueryNode('rewardsForAddress'),
      variables: { limit: 5, where: { address: { _eq: 'stake1uyp6rqthh9n7y4rng75tz85t7djy7hny35fw27say5mfxygq3er9k' } } }
    })
    const { rewards } = result.data
    expect(rewards.length).toBe(5)
    expect(rewards[0].stakePool.hash).toBeDefined()
    expect(rewards[0].earnedIn.number).toBeDefined()
  })

  it('can return aggregated data on all delegations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateRewards')
    })
    const { rewards_aggregate } = result.data
    expect(parseInt(rewards_aggregate.aggregate.avg.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.max.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.min.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.sum.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.count)).toBeGreaterThan(30000)
  })
})
