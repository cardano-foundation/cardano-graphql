/* eslint-disable camelcase */
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { gql } from 'apollo-boost'

describe('govActionProposal', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return governance action proposals', async () => {
    const result = await client.query({
      query: gql`
        query {
          govActionProposal(limit: 5) {
            tx_id
            index
            type
            deposit
            expiration
          }
        }
      `
    })
    expect(result.data.govActionProposal).toBeDefined()
    expect(result.data.govActionProposal.length).toBeGreaterThan(0)
    expect(result.data.govActionProposal[0].type).toBeDefined()
  })

  it('can filter by type', async () => {
    const result = await client.query({
      query: gql`
        query {
          govActionProposal(where: { type: { _eq: "InfoAction" } }, limit: 3) {
            tx_id
            type
          }
        }
      `
    })
    expect(result.data.govActionProposal).toBeDefined()
    result.data.govActionProposal.forEach((p: { type: string }) => {
      expect(p.type).toBe('InfoAction')
    })
  })

  it('can return aggregate count', async () => {
    const result = await client.query({
      query: gql`
        query {
          govActionProposal_aggregate {
            aggregate {
              count
            }
          }
        }
      `
    })
    expect(Number(result.data.govActionProposal_aggregate.aggregate.count)).toBeGreaterThan(0)
  })
})
