/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'token_mints'), name)
}

describe('tokenMints', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('can return information on token minting and burning', async () => {
    const result = await client.query({
      query: await loadQueryNode('tokenMints'),
      variables: {
        limit: 2
      }
    })
    const { tokenMints_aggregate, tokenMints } = result.data
    const { aggregate } = tokenMints_aggregate
    expect(aggregate.count).toBeDefined()
    expect(tokenMints.length).toBeGreaterThan(0)
    expect(parseInt(tokenMints_aggregate.aggregate.count)).toBeGreaterThan(0)
    expect(tokenMints[0].asset.fingerprint.slice(0, 5)).toBe('asset')
  })

  it('can return information on assets by fingerprint', async () => {
    const result = await client.query({
      query: await loadQueryNode('tokenMints'),
      variables: {
        where: {
          asset: { fingerprint: { _eq: 'asset1yn220qknjguqgca45muqr4jgsl20zmn4czndq5' } }
        },
        orderBy: {
          transaction: { includedAt: 'desc' }
        }
      }
    })
    const { tokenMints } = result.data
    expect(tokenMints[0].quantity).toBeDefined()
    expect(tokenMints[0].transaction.hash).toBeDefined()
    expect(tokenMints[0].asset.assetId).toBeDefined()
    expect(tokenMints[0].asset.fingerprint).toBeDefined()
    expect(tokenMints[0].asset.policyId).toBeDefined()
  })
})
