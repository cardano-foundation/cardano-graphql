import { DbConfig } from '@src/typeAliases'
import { LogLevelString } from 'bunyan'
import { MissingConfig } from '@cardano-graphql/server/dist/errors'
import fs from 'fs-extra'

export type EnvConfig = {
    url: string,
    db: DbConfig,
    loggerMinSeverity: LogLevelString
};
export async function getTestConfig (): Promise<EnvConfig> {
  const env = filterAndTypecastEnvs(process.env)
  if (!env.url) {
    throw new MissingConfig('URL env not set')
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
  let db: any
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
  return {
    url: env.url,
    db,
    loggerMinSeverity: env.loggerMinSeverity as LogLevelString || 'info' as LogLevelString
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    URL,
    POSTGRES_DB,
    POSTGRES_DB_FILE,
    POSTGRES_HOST,
    POSTGRES_PASSWORD,
    POSTGRES_PASSWORD_FILE,
    POSTGRES_PORT,
    POSTGRES_USER,
    POSTGRES_USER_FILE,
    LOGGER_MIN_SEVERITY
  } = env as NodeJS.ProcessEnv
  return {
    url: URL,
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
    loggerMinSeverity: LOGGER_MIN_SEVERITY
  }
}
