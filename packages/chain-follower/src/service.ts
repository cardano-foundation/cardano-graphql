import { createDgraphClient } from './DgraphClient'
import { getConfig } from './config'
import { Logger } from 'ts-log'
import { createLogger } from 'bunyan'
import onDeath from 'death'
import { ChainFollower } from './ChainFollower'
import { loadSchema} from './util'

(async () => {
  const config = getConfig()

  const logger: Logger = createLogger({
    name: 'chain-follower',
    level: config.loggerMinSeverity
  })

  try {
    const dgraphClient = createDgraphClient(config.dgraphAddress)
    await dgraphClient.setSchema(await loadSchema())

    const chainFollower = new ChainFollower(logger)

    onDeath(async (signal) => {
      logger.error({ signal }, 'About to exit the process')
      await chainFollower.shutdown()
      process.exit(1)
    })

    await chainFollower.initialize({ host: config.ogmiosHost, port: config.ogmiosPort })
    await chainFollower.start(['origin'])
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
