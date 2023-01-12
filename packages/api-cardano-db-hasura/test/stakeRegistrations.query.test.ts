/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_registrations'), name)
}

describe('stakeRegistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return details for stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('stakeRegistrationsSample'),
      variables: { limit: 5 }
    })
    const { stakeRegistrations } = result.data
    expect(stakeRegistrations.length).toBe(5)
    expect(stakeRegistrations[0].transaction.hash).toBeDefined()
  })

  it('can return aggregated data on all stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakeRegistrations')
    })
    const { stakeRegistrations_aggregate } = result.data
    expect(parseInt(stakeRegistrations_aggregate.aggregate.count)).toBeGreaterThan(1000)
  })
})
