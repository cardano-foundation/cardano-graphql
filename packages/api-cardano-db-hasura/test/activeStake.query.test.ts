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
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'active_stake'), name)
}

describe('activeStake', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442, genesis)
  })

  it('can return active stake snapshots for an address', async () => {
    const result = await client.query({
      query: await loadQueryNode('activeStakeForAddress'),
      variables: { limit: 5, where: { address: { _eq: 'stake1u8atejkgfn8772722rh03lmnxyshvjakk260gfsefamc6sga68ag2' } } }
    })
    const { activeStake } = result.data
    expect(activeStake.length).toBe(5)
    expect(activeStake[0].amount).toBeDefined()
    expect(activeStake[0].epochNo).toBeDefined()
    expect(activeStake[0].registeredWith.hash).toBeDefined()
  })

  it('can return aggregated active stake information for an address', async () => {
    const result = await client.query({
      query: await loadQueryNode('averageActiveStakeForAddress'),
      variables: { address: 'stake1u8atejkgfn8772722rh03lmnxyshvjakk260gfsefamc6sga68ag2' }
    })
    const { activeStake_aggregate } = result.data
    expect(activeStake_aggregate.aggregate.count).toBeDefined()
    expect(activeStake_aggregate.aggregate.avg.amount).toBeDefined()
  })
})
