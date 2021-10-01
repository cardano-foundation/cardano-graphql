import { createLogger } from 'bunyan'
import { getConfig } from './config'
import { Logger } from 'ts-log'
import { Worker } from '@cardano-graphql/worker/dist/Worker'
import { ChainFollower } from '@cardano-graphql/chain-follower'
import onDeath from 'death'
import { InMemoryClient } from '@cardano-graphql/worker'

(async () => {
  const config = getConfig()

  const logger: Logger = createLogger({
    name: 'cardano-graphql',
    level: config.loggerMinSeverity
  })

  try {
    const chainFollower = new ChainFollower(logger)

    await chainFollower.initialize(config.ogmios)

    await chainFollower.start(['origin'])

    const queueClient = await InMemoryClient()
    queueClient.subscribe('test', (msg) => console.log('msg: ', msg))

    const worker = new Worker(queueClient, logger)
    worker.start()

    onDeath(async (signal) => {
      logger.error({ signal }, 'About to exit the process')

      await worker.shutdown()

      process.exit(1)
    })
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
