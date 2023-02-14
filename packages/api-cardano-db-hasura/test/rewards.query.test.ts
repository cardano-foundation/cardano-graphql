/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init, queryDB } from './util'
import Logger from 'bunyan'
import { Client } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'rewards'), name)
}

describe('rewards', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('rewards'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })
  const getTestData = async (sql: string) => queryDB(db, logger, sql)

  it('can return details for rewards scoped to an address', async () => {
    const dbResp = await getTestData('SELECT * FROM reward JOIN stake_address sa ON reward.addr_id = sa.id WHERE amount>0 ORDER BY RANDOM() LIMIT 1;')
    const stakeAddress = dbResp.rows[0].view
    logger.info('Stake address - ' + stakeAddress)
    const result = await client.query({
      query: await loadQueryNode('rewardsForAddress'),
      variables: { limit: 5, offset: 4, where: { address: { _eq: stakeAddress } } }
    })
    const { rewards } = result.data
    expect(rewards.length).toBeGreaterThan(0)
    expect(rewards[0].stakePool.hash).toBeDefined()
    expect(rewards[0].earnedIn.number).toBeDefined()
    expect(rewards[0].receivedIn.number).toBeDefined()
    expect(rewards[0].type).toBeDefined()
  })

  it('can return aggregated data on all delegations', async () => {
    const result = await client.query({
      query: await loadQueryNode('aggregateRewards')
    })
    const { rewards_aggregate } = result.data
    expect(parseInt(rewards_aggregate.aggregate.max.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.min.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.sum.amount)).toBeDefined()
    expect(parseInt(rewards_aggregate.aggregate.count)).toBeGreaterThan(1)
  })
})
