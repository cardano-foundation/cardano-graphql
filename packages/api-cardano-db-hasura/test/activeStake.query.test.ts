/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init, queryDB } from './util'
import Logger from 'bunyan'
import { Client } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'active_stake'), name)
}

describe('activeStake', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  let stakeAddress: string

  beforeAll(async () => {
    ({ client, db, logger } = await init('activeStake'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })
  const getTestData = async (sql: string) => queryDB(db, logger, sql)

  it('can return active stake snapshots for an address', async () => {
    const dbResp = await getTestData('WITH current_epoch AS (SELECT max(epoch_no) AS epoch_no FROM block) select view from epoch_stake join stake_address on epoch_stake.addr_id = stake_address.id where epoch_no=(SELECT epoch_no FROM current_epoch) ORDER BY RANDOM() LIMIT 1;')
    stakeAddress = dbResp.rows[0].view

    logger.info('Stake address - ' + stakeAddress)
    const result = await client.query({
      query: await loadQueryNode('activeStakeForAddress'),
      variables: { limit: 5, where: { address: { _eq: stakeAddress } } }
    })
    const { activeStake } = result.data
    expect(activeStake.length).toBeLessThanOrEqual(5)
    expect(activeStake[0].amount).toBeDefined()
    expect(activeStake[0].epochNo).toBeDefined()
    expect(activeStake[0].registeredWith.hash).toBeDefined()
    expect(activeStake[0].stakePoolHash).toBeDefined()
    expect(activeStake[0].stakePoolId).toBeDefined()
  })

  it('can return aggregated active stake information for an address', async () => {
    const dbResp = await getTestData('SELECT view FROM stake_address ORDER BY RANDOM() LIMIT 1;')
    stakeAddress = dbResp.rows[0].view
    logger.info('stake address - ' + stakeAddress)
    const result = await client.query({
      query: await loadQueryNode('averageActiveStakeForAddress'),
      variables: { address: stakeAddress }
    })
    const { activeStake_aggregate } = result.data
    expect(activeStake_aggregate.aggregate.count).toBeDefined()
  })
})
