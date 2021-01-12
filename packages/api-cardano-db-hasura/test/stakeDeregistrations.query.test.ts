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
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_deregistrations'), name)
}

describe('stakeDeregistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
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
