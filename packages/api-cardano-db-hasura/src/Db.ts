import { ClientConfig } from 'pg'
import createSubscriber, { Subscriber } from 'pg-listen'
import { dummyLogger, Logger } from 'ts-log'

export class Db {
  pgSubscriber: Subscriber

  constructor (
    pgClientConfig: ClientConfig,
    private logger: Logger = dummyLogger
  ) {
    this.pgSubscriber = createSubscriber(pgClientConfig, {
      parse: (value) => value
    })
  }

  public async init ({ onDbSetup }: { onDbSetup: Function }): Promise<void> {
    this.pgSubscriber.events.on('connected', async () => {
      this.logger.debug('DbClient.pgSubscriber: Connected')
      await onDbSetup()
    })
    this.pgSubscriber.events.on('reconnect', (attempt) => {
      this.logger.warn(`DbClient.pgSubscriber: Reconnecting attempt ${attempt}`)
    })
    this.pgSubscriber.events.on('error', (error) => {
      this.logger.error('DbClient.pgSubscriber: Fatal database connection error:', error)
      process.exit(1)
    })
    this.pgSubscriber.notifications.on('cardano_db_sync_startup', async payload => {
      switch (payload) {
        case 'init' :
          this.logger.warn('DbClient.pgSubscriber: cardano-db-sync-extended starting, schema will be reset')
          break
        case 'db-setup' :
          await onDbSetup()
          break
        default :
          this.logger.error(`DbClient.pgSubscriber: Unknown message payload ${payload}`)
      }
    })
    try {
      await this.pgSubscriber.connect()
      await this.pgSubscriber.listenTo('cardano_db_sync_startup')
    } catch (error) {
      this.logger.error(error)
    }
  }

  public async shutdown () {
    await this.pgSubscriber.close()
  }
}
