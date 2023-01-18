import gql from 'graphql-tag'
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { allFieldsPopulated, init } from './util'
import { Logger } from 'ts-log'
import { Client } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'epochs'), name)
}

describe('epochs', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('epochs'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })

  it('Returns epoch details by number', async () => {
    const dbResp = await db.query('SELECT no, out_sum FROM epoch WHERE no = (SELECT max(no) FROM epoch);')
    logger.info('Epoch number -', dbResp.rows[0].no)
    const result = await client.query({
      query: await loadQueryNode('epochDetailsByNumber'),
      variables: { number: dbResp.rows[0].no }
    })
    expect(result.data.epochs[0].output).toEqual(dbResp.rows[0].out_sum)
    allFieldsPopulated(result.data.epochs[0])
  })

  it('Includes protocol params in effect for the epoch', async () => {
    const dbResp = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    logger.info('Epoch number -', dbResp.rows[0].no)
    const result = await client.query({
      query: await loadQueryNode('epochProtocolParams'),
      variables: { where: { number: { _eq: dbResp.rows[0].epoch_no } } }
    })
    allFieldsPopulated(result.data.epochs[0].protocolParams, 'extraEntropy')
  })

  it('Can return aggregated data', async () => {
    const dbResp = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    logger.info('Epoch number -', dbResp.rows[0].no)
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinEpoch'),
      variables: {
        orderBy: { number: 'asc' },
        where: { number: { _in: [1, dbResp.rows[0].epoch_no] } }
      }
    })
    allFieldsPopulated(result.data.epochs)
  })

  it('Can return filtered aggregated data', async () => {
    const dbEpoch = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    const dbSlotLeader = await db.query('SELECT description FROM slot_leader WHERE slot_leader.description IS NOT NULL ORDER BY RANDOM() LIMIT 1;')
    const dbCount = await db.query('SELECT COUNT(*) as count FROM block join slot_leader sl on block.slot_leader_id = sl.id where description = ' + '\'' + dbSlotLeader.rows[0].description + '\' and epoch_no = ' + dbEpoch.rows[0].epoch_no + ';')
    logger.info('Epoch number -', dbEpoch.rows[0].epoch_no, ', slot leader - ', dbSlotLeader.rows[0].description)
    const result = await client.query({
      query: await loadQueryNode('numberOfBlocksProducedByLeaderInEpoch'),
      variables: { number: dbEpoch.rows[0].epoch_no, slotLeader: dbSlotLeader.rows[0].description }
    })
    expect(result.data.epochs[0].number).toEqual(dbEpoch.rows[0].epoch_no)
    expect(result.data.epochs[0].blocks_aggregate.aggregate.count).toEqual(dbCount.rows[0].count)
  })

  it('Returns epoch details by number range', async () => {
    // Todo: Convert this into an actual ranged query now the performance issue is resolved.
    // TODO: how to convert ???
    const dbResp = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    const result = await client.query({
      query: await loadQueryNode('epochDetailsInRange'),
      variables: { numbers: [dbResp.rows[0].epoch_no] }
    })
    allFieldsPopulated(result.data.epochs[0])
  })

  it('Can return aggregated Epoch data', async () => {
    const dbResp = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    logger.info('Epoch number -', dbResp.rows[0].epoch_no)
    const result = await client.query({
      query: await loadQueryNode('aggregateEpochData'),
      variables: { epochNumberLessThan: dbResp.rows[0].epoch_no }
    })
    allFieldsPopulated(result.data)
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
