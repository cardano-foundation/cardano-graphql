/* eslint-disable camelcase */
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { gql } from 'apollo-boost'

describe('voteProcedure', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return vote procedures', async () => {
    const result = await client.query({
      query: gql`
        query {
          voteProcedure(limit: 5) {
            voter_role
            vote
            gov_action_proposal_id
            index
          }
        }
      `
    })
    expect(result.data.voteProcedure).toBeDefined()
    expect(result.data.voteProcedure.length).toBeGreaterThan(0)
    expect(result.data.voteProcedure[0].voter_role).toBeDefined()
    expect(result.data.voteProcedure[0].vote).toBeDefined()
  })

  it('can filter by voter role', async () => {
    const result = await client.query({
      query: gql`
        query {
          voteProcedure(where: { voter_role: { _eq: "SPO" } }, limit: 3) {
            voter_role
            vote
          }
        }
      `
    })
    expect(result.data.voteProcedure).toBeDefined()
    result.data.voteProcedure.forEach((v: { voter_role: string }) => {
      expect(v.voter_role).toBe('SPO')
    })
  })
})
