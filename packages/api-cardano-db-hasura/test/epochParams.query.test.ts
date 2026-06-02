/* eslint-disable camelcase */
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'
import { gql } from 'apollo-boost'

describe('epochParams', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.preprod()
  })

  it('can return epoch governance parameters', async () => {
    const result = await client.query({
      query: gql`
        query {
          epochParams(limit: 1, order_by: { epoch_no: desc }) {
            epoch_no
            committee_min_size
            committee_max_term_length
            gov_action_lifetime
            gov_action_deposit
            drep_deposit
            drep_activity
            dvt_motion_no_confidence
            pvt_motion_no_confidence
            pvtpp_security_group
          }
        }
      `
    })
    const { epochParams } = result.data
    expect(epochParams).toBeDefined()
    expect(epochParams.length).toBeGreaterThan(0)
    expect(epochParams[0].epoch_no).toBeDefined()
    expect(epochParams[0].committee_min_size).toBeDefined()
  })

  it('can filter by epoch number', async () => {
    const latest = await client.query({
      query: gql`
        query {
          epochParams(limit: 1, order_by: { epoch_no: desc }) {
            epoch_no
          }
        }
      `
    })
    const epochNo = latest.data.epochParams[0].epoch_no

    const result = await client.query({
      query: gql`
        query EpochParams($epoch_no: Int_comparison_exp) {
          epochParams(where: { epoch_no: $epoch_no }) {
            epoch_no
            gov_action_deposit
          }
        }
      `,
      variables: { epoch_no: { _eq: epochNo } }
    })
    expect(result.data.epochParams.length).toBe(1)
    expect(result.data.epochParams[0].epoch_no).toBe(epochNo)
  })
})
