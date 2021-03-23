import gql from 'graphql-tag'
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { epoch1, epoch220 } from './data_assertions'
import { testClient } from './util'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'epochs'), name)
}

describe('epochs', () => {
  let client: TestClient
  beforeAll(async () => {
    client = await testClient.mainnet()
  })

  it('Returns epoch details by number', async () => {
    const result = await client.query({
      query: await loadQueryNode('epochDetailsByNumber'),
      variables: { number: 1 }
    })
    expect(result.data.epochs[0]).toEqual(epoch1.basic)
    expect(result.data).toMatchSnapshot()
  })

  it('Included protocol params in effect for the epoch', async () => {
    const result = await client.query({
      query: await loadQueryNode('epochProtocolParams'),
      variables: { where: { number: { _eq: 220 } } }
    })
    expect(result.data.epochs[0].protocolParams).toEqual(epoch220.protocolParams)
    expect(result.data).toMatchSnapshot()
  })

  it('Can return aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinEpoch'),
      variables: { where: { number: { _in: [1, 220] } } }
    })
    expect(result.data.epochs[0]).toEqual(epoch1.aggregated)
    expect(result.data.epochs[1]).toEqual(epoch220.aggregated)
    expect(result.data.epochs[0].blocksCount).toEqual(epoch1.aggregated.blocks_aggregate.aggregate.count)
    expect(result.data).toMatchSnapshot()
  })

  it('Can return filtered aggregated data', async () => {
    const result = await client.query({
      query: await loadQueryNode('numberOfBlocksProducedByLeaderInEpoch'),
      variables: { number: 1, slotLeader: 'ByronGenesis-be09d88f50165c5c' }
    })
    expect(result.data).toMatchSnapshot()
  })

  it('Returns epoch details by number range', async () => {
    // Todo: Convert this into an actual ranged query now the performance issue is resolved.
    const result = await client.query({
      query: await loadQueryNode('epochDetailsInRange'),
      variables: { numbers: [1] }
    })
    expect(result.data.epochs[0]).toEqual(epoch1.basic)
    expect(result.data).toMatchSnapshot()
  })

  it('Can return aggregated Epoch data', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateEpochData'),
      variables: { epochNumberLessThan: 185 }
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
