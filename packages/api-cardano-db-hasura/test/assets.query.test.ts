/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'assets'), name)
}

describe('assets', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.mainnet()
  })

  it('can return information on assets', async () => {
    const result = await client.query({
      query: await loadQueryNode('assets'),
      variables: {
        limit: 2
      }
    })
    const { assets_aggregate, assets } = result.data
    const { aggregate } = assets_aggregate
    expect(aggregate.count).toBeDefined()
    expect(assets.length).toBeGreaterThan(0)
    expect(assets[0].tokenMints.length).toBeGreaterThan(0)
    expect(parseInt(assets[0].tokenMints_aggregate.aggregate.count)).toBeGreaterThan(0)
    expect(assets[0].fingerprint.slice(0, 5)).toBe('asset')
  })

  it('can return information on assets by fingerprint', async () => {
    const result = await client.query({
      query: await loadQueryNode('assets'),
      variables: {
        where: { fingerprint: { _eq: 'asset12h3p5l3nd5y26lr22am7y7ga3vxghkhf57zkhd' } }
      }
    })
    const { assets } = result.data
    expect(assets[0].assetId).toBeDefined()
    expect(assets[0].assetName).toBeDefined()
    expect(assets[0].description).toBeDefined()
    expect(assets[0].fingerprint).toBeDefined()
    expect(assets[0].logo).toBeDefined()
    expect(assets[0].name).toBeDefined()
    expect(assets[0].policyId).toBeDefined()
    expect(assets[0].ticker).toBeDefined()
    expect(assets[0].url).toBeDefined()
  })
})
