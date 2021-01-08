import { getConfig } from './config'
import { CompleteApiServer } from './CompleteApiServer'
import { Logger } from 'ts-log'
import { createLogger } from 'bunyan'

export * from './config'

(async function () {
  const config = await getConfig()
  const logger: Logger = createLogger({
    name: 'cardano-graphql',
    level: config.loggerLevel
  })
  try {
    const server = await CompleteApiServer(config, logger)
    await server.init()
    await server.start()
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
