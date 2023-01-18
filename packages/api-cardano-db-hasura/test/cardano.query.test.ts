import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init } from './util'
import { Client } from 'pg'
import Logger from 'bunyan'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'cardano'), name)
}

describe('cardano', () => {
  let client: TestClient
  let db: Client
  let logger: Logger
  beforeAll(async () => {
    ({ client, db, logger } = await init('cardano'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })

  it('Returns core information about the current state of the network', async () => {
    const dbTip = await db.query('SELECT max(block_no) AS block_no FROM block;')
    const dbEpoch = await db.query('SELECT max(epoch_no) AS epoch_no FROM block;')
    logger.info('Tip - ', dbTip.rows[0].block_no, ' epoch - ', dbEpoch.rows[0].epoch_no)
    const result = await client.query({
      query: await loadQueryNode('chainTipAndCurrentEpochNumber')
    })
    expect(result.data.cardano.tip.number).toBeGreaterThanOrEqual(dbTip.rows[0].block_no)
    expect(result.data.cardano.currentEpoch.number).toBeGreaterThanOrEqual(dbEpoch.rows[0].epoch_no)
  })
})
