/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'
import { Genesis } from '@src/graphql_types'

const genesis = {
  mainnet: {
    byron: require('../../../config/network/mainnet/genesis/byron.json'),
    shelley: require('../../../config/network/mainnet/genesis/shelley.json')
  }
} as Genesis

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_pools'), name)
}

describe('stakePools', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('can return details on active stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('allStakePoolFields'),
      variables: { limit: 5 }
    })
    const { stakePools } = result.data
    expect(stakePools.length).toBe(5)
    expect(stakePools[0].fixedCost).toBeDefined()
    expect(stakePools[0].hash).toBeDefined()
    expect(stakePools[0].id.slice(0, 4)).toBe('pool')
    expect(stakePools[0].margin).toBeDefined()
    expect(stakePools[0].metadataHash).toBeDefined()
    expect(stakePools[0].owners).toBeDefined()
    expect(stakePools[0].pledge).toBeDefined()
    expect(stakePools[0].relays).toBeDefined()
    expect(stakePools[0].retirements).toBeDefined()
    expect(stakePools[0].rewardAddress.slice(0, 5)).toBe('stake')
    expect(stakePools[0].rewards).toBeDefined()
    expect(stakePools[0].rewards_aggregate.aggregate.count).toBeDefined()
    expect(stakePools[0].rewards_aggregate.aggregate.sum.amount).toBeDefined()
    expect(stakePools[0].updatedIn.hash).toBeDefined()
    expect(stakePools[0].url).toBeDefined()
  })

  it('can return aggregated data on all stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary')
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(900)
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeLessThan(1500)
  })

  it('can return aggregated data on active stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary'),
      variables: { where: { _not: { retirements: {} } } }
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(800)
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeLessThan(1500)
  })

  it('can return aggregated data on retiring stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary'),
      variables: { where: { retirements: {} } }
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(0)
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeLessThan(600)
  })
})
