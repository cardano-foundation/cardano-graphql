import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'
import { Db } from '@src/Db'
import pRetry from 'p-retry'
import { gql } from 'apollo-boost'
import util from '@cardano-graphql/util'
import { HasuraClient } from '@src/HasuraClient'
import path from 'path'
import { readSecrets } from '@src/util'
import { Config } from '@src/Config'
import { Genesis } from '@src/graphql_types'

export async function buildClient (
  apiUri: string,
  hasuraUri: Config['hasuraUri'],
  dbPort: Config['db']['port'],
  genesis: Genesis
) {
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
    const hasuraClient = new HasuraClient('hasura', hasuraUri)
    const db = new Db({
      ...{ host: 'localhost', port: dbPort },
      ...await readSecrets(path.resolve(__dirname, '..', '..', '..', 'config', 'secrets'))
    })
    await db.init({
      onDbSetup: hasuraClient.applySchemaAndMetadata.bind(hasuraClient)
    })
    const schema = await buildSchema(hasuraClient, genesis)
    return utilDev.createIntegrationClient(schema)
  }
}
