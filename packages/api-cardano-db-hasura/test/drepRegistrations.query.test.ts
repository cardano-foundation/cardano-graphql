/* eslint-disable camelcase */
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { gql } from 'apollo-boost'

describe('drepRegistrations', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return drep registrations', async () => {
    const result = await client.query({
      query: gql`
        query {
          drepRegistrations(limit: 5) {
            DRepId
            deposit
            votingAnchor {
              url
              data_hash
            }
          }
        }
      `
    })
    expect(result.data.drepRegistrations).toBeDefined()
    expect(result.data.drepRegistrations.length).toBeGreaterThan(0)
    expect(result.data.drepRegistrations[0].DRepId).toBeDefined()
  })

  it('can filter by DRepId', async () => {
    const sample = await client.query({
      query: gql`
        query {
          drepRegistrations(limit: 1) {
            DRepId
          }
        }
      `
    })
    const drepId = sample.data.drepRegistrations[0].DRepId

    const result = await client.query({
      query: gql`
        query DrepRegistrations($DRepId: text_comparison_exp) {
          drepRegistrations(where: { DRepId: $DRepId }) {
            DRepId
            deposit
          }
        }
      `,
      variables: { DRepId: { _eq: drepId } }
    })
    expect(result.data.drepRegistrations.length).toBeGreaterThan(0)
    result.data.drepRegistrations.forEach((d: { DRepId: string }) => {
      expect(d.DRepId).toBe(drepId)
    })
  })
})
