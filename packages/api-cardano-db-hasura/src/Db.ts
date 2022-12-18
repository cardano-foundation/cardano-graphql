import { ClientConfig, Client } from 'pg'
import createSubscriber, { Subscriber } from 'pg-listen'
import { dummyLogger, Logger } from 'ts-log'
import { ModuleState } from '@cardano-graphql/util'

const MODULE_NAME = 'Db'

export class Db {
  private config: ClientConfig
  private state: ModuleState
  pgSubscriber: Subscriber

  constructor (
    config: ClientConfig,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.config = config
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
    await this.setupDb()
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

  private async setupDb () {
    let client: Client
    try {
      client = new Client(this.config)
      await client.connect()
      const dbQuery = await client.query("SELECT FROM pg_database WHERE datname = 'cgql'")
      if (dbQuery.rows.length === 0) {
        await client.query('CREATE DATABASE cgql')
        this.logger.info({ module: MODULE_NAME }, 'cgql DB created')
      }
      client = new Client({ ...this.config, database: 'cgql' })
      await client.connect()
      await client.query(`
        CREATE TABLE IF NOT EXISTS "Asset" (
          "assetId" BYTEA PRIMARY KEY,
          "assetName" BYTEA,
          "decimals" INT,
          "description" VARCHAR,
          "fingerprint" CHAR(44),
          "firstAppearedInSlot" INT,
          "logo" VARCHAR,
          "metadataHash" CHAR(40),
          "name" VARCHAR,
          "policyId" BYTEA,
          "ticker" VARCHAR(9),
          "url" VARCHAR
        );`
      )
    } catch (error) {
      this.logger.error({ err: error })
    } finally {
      await client?.end()
    }
  }
}
