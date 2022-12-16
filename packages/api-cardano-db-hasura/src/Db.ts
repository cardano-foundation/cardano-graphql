import { ClientConfig } from 'pg'
import createSubscriber, { Subscriber } from 'pg-listen'
import { dummyLogger, Logger } from 'ts-log'
import { ModuleState } from '@cardano-graphql/util'

const MODULE_NAME = 'Db'

export class Db {
  private state: ModuleState
  pgSubscriber: Subscriber

  constructor (
    config: ClientConfig,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.pgSubscriber = createSubscriber(config, {
      parse: (value) => value
    })
  }

  public async init ({
    onDbInit,
    onDbSetup
  }: {
    onDbInit?: () => void,
    onDbSetup: () => void
  }): Promise<void> {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing...')
    this.pgSubscriber.events.on('connected', async () => {
      this.logger.debug({ module: MODULE_NAME }, 'pgSubscriber: Connected')
      await onDbSetup()
    })
    this.pgSubscriber.events.on('reconnect', (attempt) => {
      this.logger.warn({ module: MODULE_NAME }, `pgSubscriber: Reconnecting attempt ${attempt}`)
    })
    this.pgSubscriber.events.on('error', (error) => {
      this.logger.error({ module: MODULE_NAME, err: error }, 'pgSubscriber')
      process.exit(1)
    })
    this.pgSubscriber.notifications.on('cardano_db_sync_startup', async payload => {
      switch (payload) {
        case 'init' :
          this.logger.warn({ module: 'Db' }, 'pgSubscriber: cardano-db-sync-extended starting, schema will be reset')
          await onDbInit()
          break
        case 'db-setup' :
          await onDbSetup()
          break
        default :
          this.logger.error({ module: MODULE_NAME }, `DbClient.pgSubscriber: Unknown message payload ${payload}`)
      }
    })
    try {
      await this.pgSubscriber.connect()
      await this.pgSubscriber.listenTo('cardano_db_sync_startup')
      this.state = 'initialized'
    } catch (error) {
      this.logger.error({ err: error })
    }
  }

  public async shutdown () {
    if (this.state !== 'initialized') return
    this.logger.info({ module: MODULE_NAME }, 'Shutting down...')
    await this.pgSubscriber.close()
    this.state = null
    this.logger.info({ module: MODULE_NAME }, 'Shut down')
  }
}
