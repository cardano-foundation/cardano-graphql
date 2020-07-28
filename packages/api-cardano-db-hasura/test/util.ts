import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'
import { Db } from '@src/Db'
import pRetry from 'p-retry'
import { gql } from 'apollo-boost'
import util from '@cardano-graphql/util'

export async function buildClient (apiUri: string, hasuraUri: string) {
  if (process.env.TEST_MODE === 'e2e') {
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
  } else {
    const db = new Db(hasuraUri)
    await db.init()
    const schema = await buildSchema(hasuraUri, db)
    return utilDev.createIntegrationClient(schema)
  }
}
