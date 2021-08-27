import utilDev from '@cardano-graphql/util-dev'
// import { buildSchema } from '@src/executableSchema'
// import { Db } from '@src/Db'
import pRetry from 'p-retry'
import { gql } from 'apollo-boost'
import util from '@cardano-graphql/util'
// import { HasuraClient } from '@src/HasuraClient'
// import path from 'path'
// import { readSecrets } from '@src/util'
// import { Config } from '@src/Config'
// import { Genesis } from '@src/graphql_types'
// import { CardanoNodeClient } from '@src/CardanoNodeClient'

// const getLastConfiguredMajorVersion = (network: string) =>
//   require(`../../../config/network/${network}/cardano-node/config.json`)['LastKnownBlockVersion-Major']

export const testClient = {
  mainnet: buildClient.bind(this,
    'http://localhost:3100'
    // 'http://localhost:8090',
    // 5442,
    // {
    //   byron: require('../../../config/network/mainnet/genesis/byron.json'),
    //   shelley: require('../../../config/network/mainnet/genesis/shelley.json')
    // }
  ),
  alonzoPurple: buildClient.bind(this,
    'http://localhost:3103'
    // 'http://localhost:8092',
    // 5444,
    // {
    //   byron: require('../../../config/network/alonzo-purple/genesis/byron.json'),
    //   shelley: require('../../../config/network/alonzo-purple/genesis/shelley.json')
    // }
  )
}

export async function buildClient (
  apiUri: string
  // hasuraUri: Config['hasuraUri'],
  // dbPort: Config['db']['port'],
  // genesis: Genesis,
  // lastConfiguredMajorVersion: number
) {
  // if (process.env.TEST_MODE !== 'integration') {
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
  // } else {
  //   const cardanoNodeClient = new CardanoNodeClient(
  //     genesis.shelley.protocolParams.protocolVersion.major
  //   )
  //   const hasuraClient = new HasuraClient('hasura', hasuraUri, 1000 * 60 * 5, lastConfiguredMajorVersion)
  //   const db = new Db({
  //     ...{ host: 'localhost', port: dbPort },
  //     ...await readSecrets(path.resolve(__dirname, '..', '..', '..', 'config', 'secrets'))
  //   })
  //   await db.init({
  //     onDbSetup: hasuraClient.applySchemaAndMetadata.bind(hasuraClient)
  //   })
  //   const schema = await buildSchema(hasuraClient, genesis, cardanoNodeClient)
  //   return utilDev.createIntegrationClient(schema)
  // }
}
