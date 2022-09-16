/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'withdrawals'), name)
}

describe('withdrawals', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('can return details for withdrawals', async () => {
    const result = await client.query({
      query: await loadQueryNode('withdrawalsSample'),
      variables: { limit: 5 }
    })
    const { withdrawals } = result.data
    expect(withdrawals.length).toBe(5)
    expect(withdrawals[0].address.slice(0, 5)).toBe('stake')
    expect(withdrawals[0].amount).toBeDefined()
    expect(withdrawals[0].transaction.hash).toBeDefined()
  })

  it('can return aggregated data on all withdrawals', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateWithdrawals')
    })
    const { withdrawals_aggregate } = result.data
    expect(parseInt(withdrawals_aggregate.aggregate.count)).toBeGreaterThan(200)
  })
})
