import path from 'path'
import { DocumentNode } from 'graphql'
import gql from 'graphql-tag'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { allFieldsPopulated, init } from './util'
import { Logger } from 'ts-log'
import { Client } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'blocks'), name)
}

describe('blocks', () => {
  let logger: Logger
  let client: TestClient
  let db: Client

  const getTestData = async (sql: string) => {
    const resp = await db.query(sql)
    expect(resp.rows.length).toBeGreaterThan(0)
    return resp
  }

  beforeAll(async () => {
    ({ client, db, logger } = await init('blocks'))
    await db.connect()
  })

  afterAll(async () => {
    await db.end()
  })

  it('caps the response to 100 blocks', async () => {
    const result = await client.query({
      query: await loadQueryNode('blockHashesNoArgs')
    })
    expect(result.data.blocks.length).toBe(2500)
  })

  it('allows custom pagination size with a limit and offset', async () => {
    const page1 = await client.query({
      query: await loadQueryNode('first20Blocks')
    })
    const page2 = await client.query({
      query: await loadQueryNode('second20Blocks')
    })
    expect(page1.data.blocks.length).toBe(20)
    expect(page1.data.blocks[19].number).toBe(23)
    expect(page2.data.blocks.length).toBe(20)
    expect(page2.data.blocks[19].number).toBe(43)
  })

  it('Can return blocks by number', async () => {
    const dbResp = await getTestData('SELECT block_no FROM block WHERE block_no IS NOT NULL ORDER BY RANDOM() LIMIT 2;')
    logger.info('Block numbers -', dbResp.rows[0].block_no, dbResp.rows[1].block_no)
    const result = await client.query({
      query: await loadQueryNode('blockByNumbers'),
      variables: { numbers: [dbResp.rows[0].block_no, dbResp.rows[1].block_no] }
    })
    expect(result.data.blocks.length).toBe(2)
    expect(result.data.blocks[0].hash).not.toBeNull()
    expect(result.data.blocks[1].hash).not.toBeNull()
  })

  it('Can return blocks by an array of hashes', async () => {
    const dbResp = await getTestData('SELECT hash FROM block WHERE block_no IS NOT NULL ORDER BY RANDOM() LIMIT 2;')
    logger.info('Hashes -', dbResp.rows[0].hash.toString('hex'), dbResp.rows[1].hash.toString('hex'))
    const result = await client.query({
      query: await loadQueryNode('blocksByHashes'),
      variables: {
        hashes: [
          dbResp.rows[0].hash.toString('hex'),
          dbResp.rows[1].hash.toString('hex')]
      }
    })
    expect(result.data.blocks.length).toBe(2)
    allFieldsPopulated(result.data.blocks[0])
  })

  it('Can return aggregated data', async () => {
    const dbResp = await getTestData('SELECT block_no FROM block WHERE block_no IS NOT NULL AND tx_count>0 ORDER BY RANDOM() LIMIT 1;')
    const epochResp = await getTestData('SELECT max(epoch_no) AS epoch_no FROM block;')
    logger.info('Block number -', dbResp.rows[0].block_no, ',epoch number -', epochResp.rows[0].epoch_no)
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinBlock'),
      variables: { number: dbResp.rows[0].block_no, epochLessThan: epochResp.rows[0].epoch_no }
    })
    allFieldsPopulated(result.data.blocks[0])
  })

  it('Can return filtered aggregated data', async () => {
    const dbResp = await getTestData('SELECT block_no, tx_count FROM block WHERE block_no IS NOT NULL AND tx_count > 10 ORDER BY RANDOM() LIMIT 1;')
    logger.info('Block number -', dbResp.rows[0].block_no)
    let fee = 10
    let query = gql`query {
      blocks( where: { number: { _eq: ${dbResp.rows[0].block_no} }}) {
        transactions_aggregate(
          where: {
            _and: [
              { fee: { _gt: "${fee}" }},
              { totalOutput: { _lt: "4924799478670" } }
            ]
          }) {
          aggregate {
            count
          }
        }
        number
      }
    }`
    let result = await client.query({ query })
    expect(result.data.blocks[0].transactions_aggregate.aggregate.count).toEqual(dbResp.rows[0].tx_count)

    fee = 1000000000
    query = gql`query {
      blocks( where: { number: { _eq: ${dbResp.rows[0].block_no} }}) {
        transactions_aggregate(
          where: {
            _and: [
              { fee: { _gt: "${fee}" }},
              { totalOutput: { _lt: "4924799478670" } }
            ]
          }) {
          aggregate {
            count
          }
        }
        number
      }
    }`
    result = await client.query({ query })
    expect(result.data.blocks[0].transactions_aggregate.aggregate.count).toEqual('0')
  })

  it('are linked to their predecessor, and the chain can be traversed', async () => {
    const result = await client.query({
      query: await loadQueryNode('selectGreatGrandparentBlock'),
      variables: { number: 10 }
    })
    expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toBe(7)
  })

  it('are linked to their successor, and the chain can be traversed', async () => {
    const result = await client.query({
      query: await loadQueryNode('selectGreatGrandchildBlock'),
      variables: { number: 9 }
    })
    expect(result.data.blocks[0].nextBlock.nextBlock.nextBlock.number).toBe(12)
  })
})
