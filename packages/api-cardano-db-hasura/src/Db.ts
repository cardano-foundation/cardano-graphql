import { ClientConfig } from 'pg'
import createSubscriber, { Subscriber } from 'pg-listen'

export class Db {
  pgSubscriber: Subscriber

  constructor (pgClientConfig: ClientConfig) {
    this.pgSubscriber = createSubscriber(pgClientConfig, {
      parse: (value) => value
    })
  }

  public async init ({ onDbSetup }: { onDbSetup: Function }): Promise<void> {
    this.pgSubscriber.events.on('connected', async () => {
      console.log('DbClient.pgSubscriber: Connected')
      await onDbSetup()
    })
    this.pgSubscriber.events.on('reconnect', (attempt) => {
      console.warn(`DbClient.pgSubscriber: Reconnecting attempt ${attempt}`)
    })
    this.pgSubscriber.events.on('error', (error) => {
      console.error('DbClient.pgSubscriber: Fatal database connection error:', error)
      process.exit(1)
    })
    this.pgSubscriber.notifications.on('cardano_db_sync_startup', async payload => {
      switch (payload) {
        case 'init' :
          console.log('DbClient.pgSubscriber: cardano-db-sync-extended starting, schema will be reset')
          break
        case 'db-setup' :
          await onDbSetup()
          break
        default :
          console.error(`DbClient.pgSubscriber: Unknown message payload ${payload}`)
      }
    })
    try {
      await this.pgSubscriber.connect()
      await this.pgSubscriber.listenTo('cardano_db_sync_startup')
    } catch (error) {
      console.error(error)
    }
  }

  public async shutdown () {
    await this.pgSubscriber.close()
  }
}
