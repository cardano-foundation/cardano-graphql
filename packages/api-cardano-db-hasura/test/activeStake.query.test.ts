/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { buildClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'active_stake'), name)
}

describe('activeStake', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await buildClient('http://localhost:3100', 'http://localhost:8090', 5442)
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
})
