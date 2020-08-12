/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'delegations'), name)
}

describe('delegations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442)
  }, 15000)

  it('can return details for stake delegation', async () => {
    const result = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: { limit: 5 }
    })
    const { delegations } = result.data
    expect(delegations.length).toBe(5)
    expect(delegations[0].address.slice(0, 5)).toBe('stake')
    expect(delegations[0].stakePool.hash).toBeDefined()
  })

  it('can return aggregated data on all delegations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDelegation')
    })
    const { delegations_aggregate } = result.data
    expect(parseInt(delegations_aggregate.aggregate.count)).toBeGreaterThan(900)
  })
})
