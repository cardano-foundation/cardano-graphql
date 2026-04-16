import utilDev from '@cardano-graphql/util-dev'
import pRetry from 'p-retry'
import { gql } from 'apollo-boost'
import util from '@cardano-graphql/util'

export const testClient = {
  mainnet: buildClient.bind(this,
    process.env.GRAPHQL_URL || 'http://localhost:3100'
  ),
  preprod: buildClient.bind(this,
    process.env.GRAPHQL_URL || 'http://localhost:3102'
  )
}

export async function buildClient (
  apiUri: string
) {
  const client = await utilDev.createE2EClient(apiUri)
  await pRetry(async () => {
    let result
    try {
      result = await client.query({
        query: gql`query {
              cardanoDbMeta {
                  initialized
              }}`
      })
    } catch (e) {
      throw new Error(`Cardano GraphQL Server not ready: ${e}`)
    }
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
