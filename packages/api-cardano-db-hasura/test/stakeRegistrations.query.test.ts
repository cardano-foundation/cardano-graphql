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
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_registrations'), name)
}

describe('stakeRegistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
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
    expect(parseInt(stakeRegistrations_aggregate.aggregate.count)).toBeGreaterThan(10000)
  })
})
