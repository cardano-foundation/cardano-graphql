import gql from 'graphql-tag'
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'epochs'), name)
}

describe('epochs', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.testnet()
  })

  it('Returns epoch details by number', async () => {
    const result = await client.query({
      query: await loadQueryNode('epochDetailsByNumber'),
      variables: { number: 150 }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Includes protocol params in effect for the epoch', async () => {
    const result = await client.query({
      query: await loadQueryNode('epochProtocolParams'),
      variables: { where: { number: { _eq: 166 } } }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinEpoch'),
      variables: {
        orderBy: { number: 'asc' },
        where: { number: { _in: [1, 150] } }
      }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Can return filtered aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('numberOfBlocksProducedByLeaderInEpoch'),
      variables: { number: 1, slotLeader: 'ByronGenesis-0df4205606dcb8ad' }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Returns epoch details by number range', async () => {
    // Todo: Convert this into an actual ranged query now the performance issue is resolved.
    const result = await client.query({
      query: await loadQueryNode('epochDetailsInRange'),
      variables: { numbers: [1] }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Can return aggregated Epoch data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateEpochData'),
      variables: { epochNumberLessThan: 165 }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Returns blocks scoped to epoch', async () => {
    const validQueryResult = await client.query({
      query: await loadQueryNode('blocksInEpoch'),
      variables: { number: 1, blockLimit: 1 }
    })
    const invalidQueryResult = await client.query({
      query: gql`query {
          epochs( where: { number: { _eq: 1 }}) {
              blocks(limit: 20, where: { epoch: { number: { _eq: 0 } }}) {
                  hash
              }
          }
      }`
    })
    expect(validQueryResult.data.epochs[0].blocks[0].epoch.number).toBe(1)
    expect(invalidQueryResult.data.epochs[0].blocks.length).toBe(0)
  })
})
