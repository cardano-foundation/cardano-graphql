import { Config as ApiCardanoDbHasuraConfig } from '@cardano-graphql/api-cardano-db-hasura'
import { MissingConfig } from './errors'
import fs from 'fs-extra'
import { Config as ServerConfig } from './Server'
import { LogLevelString } from 'bunyan'

export type Config = ServerConfig & ApiCardanoDbHasuraConfig & {
  loggerMinSeverity: LogLevelString
}

export async function getConfig (): Promise<Config> {
  const env = filterAndTypecastEnvs(process.env)

  if (!env.cardanoNodeConfigPath) {
    throw new MissingConfig('CARDANO_NODE_CONFIG_PATH env not set')
  }
  if (!env.hasuraCliPath) {
    throw new MissingConfig('HASURA_CLI_PATH env not set')
  }
  if (!env.hasuraUri) {
    throw new MissingConfig('HASURA_URI env not set')
  }
  if (!env.genesis.shelleyPath) {
    throw new MissingConfig('GENESIS_FILE_SHELLEY env not set')
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
  let db: Config['db']
  try {
    db = {
      database: env.postgres.db || (await fs.readFile(env.postgres.dbFile, 'utf8')).toString(),
      host: env.postgres.host,
      password: env.postgres.password || (await fs.readFile(env.postgres.passwordFile, 'utf8')).toString(),
      port: env.postgres.port,
      user: env.postgres.user || (await fs.readFile(env.postgres.userFile, 'utf8')).toString()
    }
  } catch (error) {
    throw new MissingConfig('Database configuration cannot be read')
  }
  const { postgres, ...selectedEnv } = env
  return {
    ...selectedEnv,
    allowIntrospection:
      (process.env.NODE_ENV === 'production' && env.allowIntrospection) ||
      (process.env.NODE_ENV !== 'production' && env.allowIntrospection !== false),
    allowedOrigins: env.allowedOrigins || true,
    apiPort: env.apiPort || 3100,
    cacheEnabled: env.cacheEnabled || false,
    db,
    loggerMinSeverity: env.loggerMinSeverity || 'info' as LogLevelString,
    listenAddress: env.listenAddress || '0.0.0.0',
    pollingInterval: {
      adaSupply: env.pollingInterval.adaSupply || 1000 * 60
    },
    queryDepthLimit: env.queryDepthLimit || 10
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    ALLOW_LIST_PATH,
    API_PORT,
    ASSET_METADATA_UPDATE_INTERVAL,
    CACHE_ENABLED,
    CARDANO_NODE_CONFIG_PATH,
    GENESIS_FILE_BYRON,
    GENESIS_FILE_SHELLEY,
    HASURA_CLI_PATH,
    HASURA_URI,
    LISTEN_ADDRESS,
    LOGGER_MIN_SEVERITY,
    METADATA_SERVER_URI,
    OGMIOS_HOST,
    OGMIOS_PORT,
    POLLING_INTERVAL_ADA_SUPPLY,
    POSTGRES_DB,
    POSTGRES_DB_FILE,
    POSTGRES_HOST,
    POSTGRES_PASSWORD,
    POSTGRES_PASSWORD_FILE,
    POSTGRES_PORT,
    POSTGRES_USER,
    POSTGRES_USER_FILE,
    PROMETHEUS_METRICS,
    QUERY_DEPTH_LIMIT,
    TRACING
  } = env as NodeJS.ProcessEnv
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true',
    allowedOrigins: ALLOWED_ORIGINS,
    allowListPath: ALLOW_LIST_PATH,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true',
    cardanoNodeConfigPath: CARDANO_NODE_CONFIG_PATH,
    genesis: {
      byronPath: GENESIS_FILE_BYRON,
      shelleyPath: GENESIS_FILE_SHELLEY
    },
    hasuraCliPath: HASURA_CLI_PATH,
    hasuraUri: HASURA_URI,
    listenAddress: LISTEN_ADDRESS,
    loggerMinSeverity: LOGGER_MIN_SEVERITY as LogLevelString,
    metadataUpdateInterval: {
      assets: ASSET_METADATA_UPDATE_INTERVAL ? Number(ASSET_METADATA_UPDATE_INTERVAL) : undefined
    },
    metadataServerUri: METADATA_SERVER_URI,
    ogmios: {
      host: OGMIOS_HOST,
      port: OGMIOS_PORT ? Number(OGMIOS_PORT) : undefined
    },
    pollingInterval: {
      adaSupply: Number(POLLING_INTERVAL_ADA_SUPPLY)
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
    prometheusMetrics: PROMETHEUS_METRICS === 'true',
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true'
  }
}
