import { createLogger, LogLevelString } from 'bunyan'
import { Db, HasuraBackgroundClient, MetadataClient, Worker } from './index'
import onDeath from 'death'
import { Logger } from 'ts-log'
import { CustomError } from 'ts-custom-error'
import fs from 'fs-extra'
import { DbConfig } from './typeAliases'
import { AssetCreator } from './AssetCreator'
// Todo: Hoist to util package next major version
export class MissingConfig extends CustomError {
  public constructor (message: string) {
    super()
    this.message = message
  }
}

export interface BackgroundConfig {
  db: DbConfig,
  hasuraCliPath: string,
  hasuraCliExtPath: string,
  hasuraUri: string,
  chainfollower?: {
    id: string,
    slot: number
  }
  loggerMinSeverity: LogLevelString,
  metadataServerUri: string,
  metadataUpdateInterval?: {
    assets: number
  },
  ogmios?: {
    host?: string
    port?: number
  }
}

async function getConfig (): Promise<BackgroundConfig> {
  const env = filterAndTypecastEnvs(process.env)
  if (!env.hasuraCliPath) {
    throw new MissingConfig('HASURA_CLI_PATH env not set')
  }
  if (!env.hasuraCliExtPath) {
    throw new MissingConfig('HASURA_CLI_EXT_PATH env not set')
  }
  if (!env.hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
  }
  if (!env.metadataServerUri) {
    throw new MissingConfig('METADATA_SERVER_URI env not set')
  }
  if (!env.postgres.dbFile && !env.postgres.db) {
    throw new MissingConfig('POSTGRES_DB_FILE or POSTGRES_DB env not set')
  }
  if (!env.postgres.host) {
    throw new MissingConfig('POSTGRES_HOST env not set')
  }
  if (!env.postgres.passwordFile && !env.postgres.password) {
    throw new MissingConfig('POSTGRES_PASSWORD_FILE or POSTGRES_PASSWORD env not set')
  }
  if (!env.postgres.port) {
    throw new MissingConfig('POSTGRES_PORT env not set')
  }
  if (!env.postgres.userFile && !env.postgres.user) {
    throw new MissingConfig('POSTGRES_USER_FILE or POSTGRES_USER env not set')
  }
  let db: BackgroundConfig['db']
  try {
    db = {
      database: env.postgres.db || (await fs.readFile(env.postgres.dbFile, 'utf8')).toString().trim(),
      host: env.postgres.host,
      password: env.postgres.password || (await fs.readFile(env.postgres.passwordFile, 'utf8')).toString().trim(),
      port: env.postgres.port,
      user: env.postgres.user || (await fs.readFile(env.postgres.userFile, 'utf8')).toString().trim()
    }
  } catch (error) {
    throw new MissingConfig('Database configuration cannot be read')
  }
  let chainfollower
  if (env.chainfollower) {
    chainfollower = {
      id: env.chainfollower.id,
      slot: env.chainfollower.slot
    }
  }
  const { postgres, ...selectedEnv } = env
  return {
    ...selectedEnv,
    db,
    chainfollower,
    loggerMinSeverity: env.loggerMinSeverity || 'info' as LogLevelString
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ASSET_METADATA_UPDATE_INTERVAL,
    HASURA_CLI_PATH,
    HASURA_CLI_EXT_PATH,
    HASURA_URI,
    LOGGER_MIN_SEVERITY,
    METADATA_SERVER_URI,
    OGMIOS_HOST,
    OGMIOS_PORT,
    POSTGRES_DB,
    POSTGRES_DB_FILE,
    POSTGRES_HOST,
    POSTGRES_PASSWORD,
    POSTGRES_PASSWORD_FILE,
    POSTGRES_PORT,
    POSTGRES_USER,
    POSTGRES_USER_FILE,
    CHAIN_FOLLOWER_START_ID,
    CHAIN_FOLLOWER_START_SLOT
  } = env as NodeJS.ProcessEnv
  return {
    hasuraCliPath: HASURA_CLI_PATH,
    hasuraCliExtPath: HASURA_CLI_EXT_PATH,
    hasuraUri: HASURA_URI,
    loggerMinSeverity: LOGGER_MIN_SEVERITY as LogLevelString,
    metadataServerUri: METADATA_SERVER_URI,
    metadataUpdateInterval: {
      assets: ASSET_METADATA_UPDATE_INTERVAL ? Number(ASSET_METADATA_UPDATE_INTERVAL) : undefined
    },
    ogmios: {
      host: OGMIOS_HOST,
      port: OGMIOS_PORT ? Number(OGMIOS_PORT) : undefined
    },
    postgres: {
      db: POSTGRES_DB,
      dbFile: POSTGRES_DB_FILE,
      host: POSTGRES_HOST,
      password: POSTGRES_PASSWORD,
      passwordFile: POSTGRES_PASSWORD_FILE,
      port: POSTGRES_PORT ? Number(POSTGRES_PORT) : undefined,
      user: POSTGRES_USER,
      userFile: POSTGRES_USER_FILE
    },
    chainfollower: {
      id: CHAIN_FOLLOWER_START_ID,
      slot: CHAIN_FOLLOWER_START_SLOT ? Number(CHAIN_FOLLOWER_START_SLOT) : undefined
    }
  }
}

(async function () {
  const config = await getConfig()
  const logger: Logger = createLogger({
    name: 'background',
    level: config.loggerMinSeverity
  })
  try {
    const hasuraBackgroundClient = new HasuraBackgroundClient(
      config.hasuraCliPath,
      config.hasuraCliExtPath,
      config.hasuraUri,
      logger
    )

    const db = new Db(config.db, logger)

    const assetCreator = new AssetCreator(
      logger,
      hasuraBackgroundClient,
      config.db
    )

    const metadataClient = new MetadataClient(
      config.metadataServerUri,
      logger
    )
    const worker = new Worker(
      hasuraBackgroundClient,
      logger,
      metadataClient,
      config.db,
      {
        metadataUpdateInterval: {
          assets: config.metadataUpdateInterval?.assets
        }
      }
    )

    await db.init({
      onDbInit: () => hasuraBackgroundClient.shutdown(),
      onDbSetup: async () => {
        try {
          await hasuraBackgroundClient.initialize()
          await assetCreator.initialize()
          await metadataClient.initialize()
          await worker.start()
          await assetCreator.start()
        } catch (error) {
          logger.error(error.message)
          process.exit(1)
        }
      }
    })
    onDeath(async () => {
      await Promise.all([
        hasuraBackgroundClient.shutdown,
        worker.shutdown,
        db.shutdown
      ])
      process.exit(1)
    })
  } catch (error) {
    logger.error('Exiting due to uncaught exception', error.message)
    process.exit(1)
  }
})()
