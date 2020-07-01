import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'

export async function buildClient () {
  if (process.env.TEST_MODE === 'e2e') {
    return utilDev.createE2EClient()
  } else {
    const schema = await buildSchema('http://localhost:8090')
    return utilDev.createIntegrationClient(schema)
  }
}
