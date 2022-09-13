import { createLogger, LogLevelString } from 'bunyan'
import { Db, HasuraManagementClient } from '../index'
import onDeath from 'death'
import { Logger } from 'ts-log'
import { CustomError } from 'ts-custom-error'
import fs from 'fs-extra'
import { DbConfig } from '../typeAliases'
// Todo: Hoist to util package next major version
export class MissingConfig extends CustomError {
  public constructor (message: string) {
    super()
    this.message = message
  }
}

export interface DbManagerConfig {
  db: DbConfig,
  hasuraCliPath: string,
  hasuraUri: string,
  loggerMinSeverity: LogLevelString
}

async function getConfig (): Promise<DbManagerConfig> {
  const env = filterAndTypecastEnvs(process.env)
  if (!env.hasuraCliPath) {
    throw new MissingConfig('HASURA_CLI_PATH env not set')
  }
  if (!env.hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
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
  let db: DbManagerConfig['db']
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
  const { postgres, ...selectedEnv } = env
  return {
    ...selectedEnv,
    db,
    loggerMinSeverity: env.loggerMinSeverity || 'info' as LogLevelString
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    HASURA_CLI_PATH,
    HASURA_URI,
    LOGGER_MIN_SEVERITY,
    POSTGRES_DB,
    POSTGRES_DB_FILE,
    POSTGRES_HOST,
    POSTGRES_PASSWORD,
    POSTGRES_PASSWORD_FILE,
    POSTGRES_PORT,
    POSTGRES_USER,
    POSTGRES_USER_FILE
  } = env as NodeJS.ProcessEnv
  return {
    hasuraCliPath: HASURA_CLI_PATH,
    hasuraUri: HASURA_URI,
    loggerMinSeverity: LOGGER_MIN_SEVERITY as LogLevelString,
    postgres: {
      db: POSTGRES_DB,
      dbFile: POSTGRES_DB_FILE,
      host: POSTGRES_HOST,
      password: POSTGRES_PASSWORD,
      passwordFile: POSTGRES_PASSWORD_FILE,
      port: POSTGRES_PORT ? Number(POSTGRES_PORT) : undefined,
      user: POSTGRES_USER,
      userFile: POSTGRES_USER_FILE
    }
  }
}

(async function () {
  const config = await getConfig()
  const logger: Logger = createLogger({
    name: 'db-manager',
    level: config.loggerMinSeverity
  })
  try {
    const hasuraManagementClient = new HasuraManagementClient(
      config.hasuraCliPath,
      config.hasuraUri,
      logger
    )
    const db = new Db(config.db, logger)
    await db.init({
      onDbInit: () => hasuraManagementClient.shutdown(),
      onDbSetup: async () => {
        try {
          await hasuraManagementClient.initialize()
        } catch (error) {
          process.exit(1)
        }
      }
    })
    await hasuraManagementClient.initialize()
    onDeath(async () => {
      await hasuraManagementClient.shutdown()
      await db.shutdown()
      process.exit(1)
    })
  } catch (error) {
    logger.error(error)
    process.exit(1)
  }
})()
