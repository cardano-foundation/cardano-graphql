/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_registrations'), name)
}

describe('stakeRegistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    ({ client } = await init('stakeRegistrations'))
  })

  it('can return details for stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('stakeRegistrationsSample'),
      variables: { limit: 3 }
    })
    const { stakeRegistrations } = result.data
    expect(stakeRegistrations.length).toBe(3)
    expect(stakeRegistrations[0].transaction.hash).toBeDefined()
  })

  it('can return aggregated data on all stake registrations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakeRegistrations')
    })
    const { stakeRegistrations_aggregate } = result.data
    expect(parseInt(stakeRegistrations_aggregate.aggregate.count)).toBeGreaterThan(1)
  })
})
