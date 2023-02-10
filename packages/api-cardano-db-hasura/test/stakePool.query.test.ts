/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init } from './util'
import Logger from 'bunyan'
import { Client, QueryResult } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'stake_pools'), name)
}

describe('stakePools', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('stakePools'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })
  const getTestData = async (sql: string) :Promise<QueryResult> => {
    const resp = await db.query(sql)
    if (resp.rows.length === 0) logger.error('Can not find suitable data in db')
    expect(resp.rows.length).toBeGreaterThan(0)
    return resp
  }

  it('can lookup stake pools by ID', async () => {
    const dbResp = await getTestData('SELECT view FROM pool_hash ORDER BY RANDOM() LIMIT 1;')
    const poolId = dbResp.rows[0].view
    logger.info('Stake pool id - ' + poolId)
    const result = await client.query({
      query: await loadQueryNode('stakePoolById'),
      variables: { id: poolId }
    })
    const { stakePools } = result.data
    expect(stakePools.length).toBe(1)
    expect(stakePools[0].hash).toBeDefined()
  })

  it('can return details on active stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('allStakePoolFields'),
      variables: {
        limit: 3,
        blocksLimit: 3,
        delegatorsLimit: 3,
        activeStakeLimit: 3
      }
    })
    const { stakePools } = result.data
    expect(stakePools.length).toBe(3)
    expect(stakePools[0].activeStake).toBeDefined()
    expect(stakePools[0].activeStake_aggregate).toBeDefined()
    expect(stakePools[0].blocks).toBeDefined()
    expect(stakePools[0].blocks_aggregate).toBeDefined()
    expect(stakePools[0].delegators).toBeDefined()
    expect(stakePools[0].delegators_aggregate).toBeDefined()
    expect(stakePools[0].fixedCost).toBeDefined()
    expect(stakePools[0].hash).toBeDefined()
    expect(stakePools[0].id.slice(0, 4)).toBe('pool')
    expect(stakePools[0].margin).toBeDefined()
    expect(stakePools[0].metadataHash).toBeDefined()
    expect(stakePools[0].owners).toBeDefined()
    expect(stakePools[0].pledge).toBeDefined()
    expect(stakePools[0].relays).toBeDefined()
    expect(stakePools[0].retirements).toBeDefined()
    expect(stakePools[0].rewardAddress.slice(0, 5)).toBe('stake')
    expect(stakePools[0].rewards).toBeDefined()
    expect(stakePools[0].rewards_aggregate.aggregate.count).toBeDefined()
    expect(stakePools[0].rewards_aggregate.aggregate.sum.amount).toBeDefined()
    expect(stakePools[0].updatedIn.hash).toBeDefined()
    expect(stakePools[0].url).toBeDefined()
  })

  it('can return aggregated data on all stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary')
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(2)
  })

  it('can return aggregated data on active stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary'),
      variables: { where: { _not: { retirements: {} } } }
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(2)
  })

  it('can return aggregated data on retiring stake pools', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateStakePoolSummary'),
      variables: { where: { retirements: {} } }
    })
    const { stakePools_aggregate } = result.data
    expect(parseInt(stakePools_aggregate.aggregate.count)).toBeGreaterThan(0)
  })
})
