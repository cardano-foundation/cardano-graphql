/* eslint-disable camelcase */
import path from 'path'

import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { init } from './util'
import { Logger } from 'ts-log'
import { Client, QueryResult } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'assets'), name)
}

describe('assets', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('assets'))
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

  it('can return information on assets', async () => {
    const result = await client.query({
      query: await loadQueryNode('assets'),
      variables: {
        limit: 2
      }
    })
    const { assets_aggregate, assets } = result.data
    const { aggregate } = assets_aggregate
    expect(aggregate.count).toBeDefined()
    expect(assets.length).toBeGreaterThan(0)
    expect(assets[0].tokenMints.length).toBeGreaterThan(0)
    expect(parseInt(assets[0].tokenMints_aggregate.aggregate.count)).toBeGreaterThan(0)
    expect(assets[0].fingerprint.slice(0, 5)).toBe('asset')
  })

  it('can return information on assets by fingerprint', async () => {
    const dbResp = await getTestData('SELECT fingerprint FROM "Asset" ORDER BY RANDOM() LIMIT 1;')
    const assetFingerprint = dbResp.rows[0].fingerprint
    logger.info('Asset fingerprint - ' + assetFingerprint)
    const result = await client.query({
      query: await loadQueryNode('assets'),
      variables: {
        where: {
          fingerprint: { _eq: assetFingerprint }
        }
      }
    })
    const { assets } = result.data
    expect(assets[0].assetId).toBeDefined()
    expect(assets[0].assetName).toBeDefined()
    expect(assets[0].description).toBeDefined()
    expect(assets[0].fingerprint).toBeDefined()
    expect(assets[0].logo).toBeDefined()
    expect(assets[0].name).toBeDefined()
    expect(assets[0].policyId).toBeDefined()
    expect(assets[0].ticker).toBeDefined()
    expect(assets[0].url).toBeDefined()
  })
  it('can return information on assets by assetId', async () => {
    const dbResp = (await getTestData('SELECT * FROM "Asset" ORDER BY RANDOM() LIMIT 1;')).rows[0]
    const assetId = dbResp.assetId.toString('hex')
    logger.info('assetId - ' + assetId)
    const result = await client.query({
      query: await loadQueryNode('assets'),
      variables: {
        where: {
          _and: [{
            assetId: { _eq: assetId }
          }]
        }
      }
    })
    const { assets } = result.data
    expect(assets[0].assetName).toBe(dbResp.assetName.toString('hex'))
    expect(assets[0].decimals).toBe(dbResp.decimals)
    expect(assets[0].description).toBe(dbResp.description)
    expect(assets[0].fingerprint).toBe(dbResp.fingerprint)
    expect(assets[0].logo).toBe(dbResp.logo)
    expect(assets[0].name).toBe(dbResp.name)
    expect(assets[0].policyId).toBe(dbResp.policyId.toString('hex'))
    expect(assets[0].ticker).toBe(dbResp.ticker)
    expect(assets[0].url).toBe(dbResp.url)
  })
})
