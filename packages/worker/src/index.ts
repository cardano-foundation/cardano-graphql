import { createLogger } from 'bunyan'
import { Logger } from 'ts-log'
import { getConfig } from './config'
import { Worker } from './Worker'
import onDeath from 'death'
import { RabbitMqClient } from './Queue'

(async () => {
  const config = getConfig()

  const logger: Logger = createLogger({
    name: 'worker',
    level: config.loggerMinSeverity
  })

  try {
    const queueClient = await RabbitMqClient()

    const worker = new Worker(queueClient, logger)

    onDeath(async (signal) => {
      logger.error({ signal }, 'About to exit the process')

      await worker.shutdown()

      process.exit(1)
    })

    worker.start()
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
