import utilDev from '@cardano-graphql/util-dev'
import pRetry from 'p-retry'
import { gql } from 'apollo-boost'
import util from '@cardano-graphql/util'
import { Client, QueryResult } from 'pg'
import { getTestConfig } from './env.config'
import Logger, { createLogger } from 'bunyan'

export async function buildClient (
  apiUri: string
) {
  const client = await utilDev.createE2EClient(apiUri)
  await pRetry(async () => {
    const result = await client.query({
      query: gql`query {
                cardanoDbMeta {
                    initialized
                }}`
    })
    if (result.data?.cardanoDbMeta.initialized === false) {
      throw new Error(`Cardano DB is not initialized: ${JSON.stringify(result.data)}`)
    }
  }, {
    factor: 1.75,
    retries: 9,
    onFailedAttempt: util.onFailedAttemptFor('Cardano GraphQL Server readiness')
  })
  return client
}

export async function init (name: string) {
  const config = await getTestConfig()
  const gqlClient = await buildClient(config.url)
  const dbClient: Client = new Client({
    user: config.db.user,
    password: config.db.password,
    database: config.db.database,
    port: config.db.port
  })
  const logger = createLogger({
    name,
    level: config.loggerMinSeverity
  })
  return {
    client: gqlClient,
    db: dbClient,
    logger
  }
}

export const queryDB = async (db: Client, logger: Logger, sql: string) :Promise<QueryResult> => {
  const resp = await db.query(sql)
  if (resp.rows.length === 0) logger.error('Can not find suitable data in db')
  expect(resp.rows.length).toBeGreaterThan(0)
  return resp
}

export const allFieldsPopulated = (obj: any, except : any = []) => {
  expect(obj).toBeDefined()
  if (Array.isArray(obj)) {
    obj.map(allFieldsPopulated)
  } else {
    let k: keyof typeof obj
    for (k in obj) {
      if (
        typeof obj[k] === 'object'
      ) {
        allFieldsPopulated(obj[k], except)
      }
      if (except.length > 0 && !except.includes(k)) {
        if (obj[k] === null) {
          throw new Error('field ' + k + ' is null')
        }
      }
      if (except.length === 0) {
        if (obj[k] === null) {
          throw new Error('field ' + k + ' is null')
        }
      }
    }
  }
}
