import { createLogger, LogLevelString } from 'bunyan'
import fetch from 'node-fetch'
import pRetry from 'p-retry'
import { HasuraBackgroundClient } from './index'

;(async function () {
  const {
    HASURA_CLI_PATH,
    HASURA_CLI_EXT_PATH,
    HASURA_URI,
    LOGGER_MIN_SEVERITY
  } = process.env as NodeJS.ProcessEnv

  if (!HASURA_CLI_PATH || !HASURA_CLI_EXT_PATH || !HASURA_URI) {
    console.error('Missing required env: HASURA_CLI_PATH, HASURA_CLI_EXT_PATH, HASURA_URI')
    process.exit(1)
  }

  const logger = createLogger({
    name: 'hasura-setup',
    level: (LOGGER_MIN_SEVERITY || 'info') as LogLevelString
  })

  const client = new HasuraBackgroundClient(
    HASURA_CLI_PATH,
    HASURA_CLI_EXT_PATH,
    HASURA_URI,
    logger
  )

  await client.applySchemaAndMetadata()

  logger.info({ module: 'hasura-setup' }, 'Verifying schema is ready')
  await pRetry(
    async () => {
      const result = await fetch(`${HASURA_URI}/v1/graphql`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-Hasura-Role': 'cardano-graphql'
        },
        body: JSON.stringify({ query: '{ __type(name: "query_root") { fields { name } } }' })
      })
      const { data } = await result.json() as any
      const fields: string[] = (data?.__type?.fields ?? []).map((f: any) => f.name)
      if (!fields.includes('epochParams')) {
        throw new Error('epochParams not yet visible in Hasura schema')
      }
    },
    {
      factor: 1.5,
      retries: 10,
      onFailedAttempt: (e) => logger.debug({ module: 'hasura-setup' }, e.message)
    }
  )

  logger.info({ module: 'hasura-setup' }, 'Schema ready')
  process.exit(0)
})()
