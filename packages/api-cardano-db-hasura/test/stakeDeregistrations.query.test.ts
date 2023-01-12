/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_deregistrations'), name)
}

describe('stakeDeregistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return details for stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('stakeDeregistrationsSample'),
      variables: { limit: 5 }
    })
    const { stakeDeregistrations } = result.data
    expect(stakeDeregistrations.length).toBe(5)
    expect(stakeDeregistrations[0].transaction.hash).toBeDefined()
  })

  it('can return aggregated data on all stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakeDeregistrations')
    })
    const { stakeDeregistrations_aggregate } = result.data
    expect(parseInt(stakeDeregistrations_aggregate.aggregate.count)).toBeGreaterThan(10)
  })
})
