/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { gql } from 'apollo-boost'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'delegation_votes'), name)
}

describe('delegationVotes', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return delegation votes with drep info', async () => {
    const result = await client.query({
      query: await loadQueryNode('delegationVotesSample'),
      variables: { limit: 5 }
    })
    const { delegationVotes } = result.data
    expect(delegationVotes.length).toBeGreaterThan(0)
    expect(delegationVotes[0].address.slice(0, 5)).toBe('stake')
    expect(delegationVotes[0].drep).toBeDefined()
    expect(delegationVotes[0].transaction.block.number).toBeDefined()
  })

  it('can filter delegation votes by address', async () => {
    const sampleResult = await client.query({
      query: await loadQueryNode('delegationVotesSample'),
      variables: { limit: 1 }
    })
    const address = sampleResult.data.delegationVotes[0].address

    const filtered = await client.query({
      query: await loadQueryNode('delegationVotesSample'),
      variables: {
        limit: 10,
        where: { address: { _eq: address } }
      }
    })
    const { delegationVotes } = filtered.data
    expect(delegationVotes.length).toBeGreaterThan(0)
    expect(delegationVotes.every((dv: { address: string }) => dv.address === address)).toBe(true)
  })

  it('can return drep hashes', async () => {
    const result = await client.query({
      query: gql`
        query {
          drepHashes(limit: 5) {
            view
            has_script
            raw
          }
        }
      `
    })
    const { drepHashes } = result.data
    expect(drepHashes.length).toBeGreaterThan(0)
    expect(drepHashes[0].view).toBeDefined()
  })

  it('can return off-chain drep data', async () => {
    const result = await client.query({
      query: gql`
        query {
          offChainVoteDrepData(limit: 5) {
            given_name
            motivations
            objectives
            qualifications
            payment_address
          }
        }
      `
    })
    expect(result.data.offChainVoteDrepData).toBeDefined()
    // preprod has registered DREPs with off-chain metadata
    expect(result.data.offChainVoteDrepData.length).toBeGreaterThan(0)
  })
})
