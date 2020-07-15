import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'
import { Db } from '@src/Db'

export async function buildClient () {
  if (process.env.TEST_MODE === 'e2e') {
    return utilDev.createE2EClient()
  } else {
    const hasuraUri = 'http://localhost:8090'
    const db = new Db(hasuraUri)
    await db.init()
    const schema = await buildSchema(hasuraUri, db)
    return utilDev.createIntegrationClient(schema)
  }
}
