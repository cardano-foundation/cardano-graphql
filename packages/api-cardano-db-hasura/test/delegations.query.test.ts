/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'delegations'), name)
}

describe('delegations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return details for stake delegation', async () => {
    const result = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: { limit: 5 }
    })
    const { delegations } = result.data
    expect(delegations.length).toBe(5)
    expect(delegations[0].address.slice(0, 5)).toBe('stake')
    expect(delegations[0].stakePool.hash).toBeDefined()
    expect(delegations[0].transaction.block.number).toBeDefined()
  })

  it('can filter delegations by stake address', async () => {
    const sampleResult = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: { limit: 1 }
    })
    const address = sampleResult.data.delegations[0].address

    const filtered = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: {
        limit: 10,
        where: { address: { _eq: address } }
      }
    })
    const { delegations } = filtered.data
    expect(delegations.length).toBeGreaterThan(0)
    expect(delegations.every((d: { address: string }) => d.address === address)).toBe(true)
  })

  it('can filter delegations by stake pool hash', async () => {
    const sampleResult = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: { limit: 1 }
    })
    const poolHash = sampleResult.data.delegations[0].stakePool.hash

    const filtered = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: {
        limit: 5,
        where: { stakePool: { hash: { _eq: poolHash } } }
      }
    })
    const { delegations } = filtered.data
    expect(delegations.length).toBeGreaterThan(0)
    expect(delegations.every((d: { stakePool: { hash: string } }) => d.stakePool.hash === poolHash)).toBe(true)
  })

  it('can return transaction details for a delegation', async () => {
    const result = await client.query({
      query: await loadQueryNode('delegationSample'),
      variables: { limit: 1 }
    })
    const delegation = result.data.delegations[0]
    expect(delegation.transaction.fee).toBeDefined()
    expect(delegation.transaction.totalOutput).toBeDefined()
    expect(parseInt(delegation.transaction.fee)).toBeGreaterThan(0)
  })

  it('can return aggregated data on all delegations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDelegation')
    })
    const { delegations_aggregate } = result.data
    expect(parseInt(delegations_aggregate.aggregate.count)).toBeGreaterThan(900)
  })
})
