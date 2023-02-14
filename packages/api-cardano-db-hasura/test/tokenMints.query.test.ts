/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init, queryDB } from './util'
import Logger from 'bunyan'
import { Client } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'token_mints'), name)
}

describe('tokenMints', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('tokenMints'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })
  const getTestData = async (sql: string) => queryDB(db, logger, sql)

  it('can return information on token minting and burning', async () => {
    const result = await client.query({
      query: await loadQueryNode('tokenMints'),
      variables: {
        limit: 2
      }
    })
    const { tokenMints_aggregate, tokenMints } = result.data
    const { aggregate } = tokenMints_aggregate
    expect(aggregate.count).toBeDefined()
    expect(tokenMints.length).toBeGreaterThan(0)
    expect(parseInt(tokenMints_aggregate.aggregate.count)).toBeGreaterThan(0)
    expect(tokenMints[0].asset.fingerprint.slice(0, 5)).toBe('asset')
  })

  it('can return information on assets by fingerprint', async () => {
    const dbResp = await getTestData('SELECT fingerprint FROM "Asset" ORDER BY RANDOM() LIMIT 1;')
    const fingerprint = dbResp.rows[0].fingerprint
    logger.info('Fingerprint - ' + fingerprint)
    const result = await client.query({
      query: await loadQueryNode('tokenMints'),
      variables: {
        where: {
          asset: { fingerprint: { _eq: fingerprint } }
        },
        limit: 10,
        offset: 0
      }
    })
    const { tokenMints } = result.data
    expect(tokenMints[0].quantity).toBeDefined()
    expect(tokenMints[0].transaction.hash).toBeDefined()
    expect(tokenMints[0].asset.assetId).toBeDefined()
    expect(tokenMints[0].asset.fingerprint).toBeDefined()
    expect(tokenMints[0].asset.policyId).toBeDefined()
  })
})
