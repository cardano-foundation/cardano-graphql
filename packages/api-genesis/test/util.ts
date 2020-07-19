import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'
import { Genesis } from '@src/graphql_types'

export async function buildClient (genesis: Genesis) {
  if (process.env.TEST_MODE === 'e2e') {
    return utilDev.createE2EClient()
  } else {
    const schema = await buildSchema(genesis)
    return utilDev.createIntegrationClient(schema)
  }
}
