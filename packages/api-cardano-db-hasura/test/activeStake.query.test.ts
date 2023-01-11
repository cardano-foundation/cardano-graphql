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
      variables: { limit: 5, where: { address: { _eq: 'stake_test1upxue2rk4tp0e3tp7l0nmfmj6ar7y9yvngzu0vn7fxs9ags2apttt' } } }
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
      variables: { address: 'stake_test1uq4l6kqvvhxywxxae04u4g6uv9sa0yymscuql5an693p53g4qz4rk' }
    })
    const { activeStake_aggregate } = result.data
    expect(activeStake_aggregate.aggregate.count).toBeDefined()
    expect(activeStake_aggregate.aggregate.avg.amount).toBeDefined()
  })
})
