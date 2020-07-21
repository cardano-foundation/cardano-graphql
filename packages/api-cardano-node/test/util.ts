import utilDev from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/executableSchema'

export async function buildClient () {
  if (process.env.TEST_MODE === 'e2e') {
    return utilDev.createE2EClient()
  } else {
    const cardanoCli = process.env.CARDANO_CLI_CMD ? process.env.CARDANO_CLI_CMD : 'cardano-cli'
    const testnet = `--testnet-magic ${process.env.CARDANO_MAGIC || '42'}`
    const settings = { testnet, cardanoCli }
    const schema = buildSchema(settings)
    return utilDev.createIntegrationClient(schema)
  }
}
