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

  if (!env.cardanoNodeSocketPath) {
    throw new MissingConfig('CARDANO_NODE_SOCKET_PATH env not set')
  }
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
  if (!env.postgresDbFile && !env.postgresDb) {
    throw new MissingConfig('POSTGRES_DB_FILE or POSTGRES_DB env not set')
  }
  if (!env.postgresHost) {
    throw new MissingConfig('POSTGRES_HOST env not set')
  }
  if (!env.postgresPasswordFile && !env.postgresPassword) {
    throw new MissingConfig('POSTGRES_PASSWORD_FILE or POSTGRES_PASSWORD env not set')
  }
  if (!env.postgresPort) {
    throw new MissingConfig('POSTGRES_PORT env not set')
  }
  if (!env.postgresUserFile && !env.postgresUser) {
    throw new MissingConfig('POSTGRES_USER_FILE or POSTGRES_USER env not set')
  }
  let db: Config['db']
  try {
    db = {
      database: env.postgresDb || (await fs.readFile(env.postgresDbFile, 'utf8')).toString(),
      host: env.postgresHost,
      password: env.postgresPassword || (await fs.readFile(env.postgresPasswordFile, 'utf8')).toString(),
      port: env.postgresPort,
      user: env.postgresUser || (await fs.readFile(env.postgresUserFile, 'utf8')).toString()
    }
  } catch (error) {
    throw new MissingConfig('Database configuration cannot be read')
  }
  return {
    ...env,
    allowIntrospection:
      (process.env.NODE_ENV === 'production' && env.allowIntrospection) ||
      (process.env.NODE_ENV !== 'production' && env.allowIntrospection !== false),
    allowedOrigins: env.allowedOrigins || true,
    apiPort: env.apiPort || 3100,
    cacheEnabled: env.cacheEnabled || false,
    cardanoCliPath: env.cardanoCliPath || 'cardano-cli',
    db,
    loggerMinSeverity: env.loggerMinSeverity || 'info' as LogLevelString,
    jqPath: env.jqPath || 'jq',
    listenAddress: env.listenAddress || '0.0.0.0',
    pollingInterval: {
      adaSupply: env.pollingInterval.adaSupply || 1000 * 60,
      metadataSync: {
        initial: env.pollingInterval.metadataSync.initial || 1000 * 60 * 5,
        ongoing: env.pollingInterval.metadataSync.ongoing || 1000 * 60 * 60
      }
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
    CACHE_ENABLED,
    CARDANO_CLI_PATH,
    CARDANO_NODE_CONFIG_PATH,
    CARDANO_NODE_SOCKET_PATH,
    GENESIS_FILE_BYRON,
    GENESIS_FILE_SHELLEY,
    HASURA_CLI_PATH,
    HASURA_URI,
    JQ_PATH,
    LISTEN_ADDRESS,
    LOGGER_MIN_SEVERITY,
    METADATA_SERVER_URI,
    POLLING_INTERVAL_ADA_SUPPLY,
    POLLING_INTERVAL_METADATA_SYNC_INITIAL,
    POLLING_INTERVAL_METADATA_SYNC_ONGOING,
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
    cardanoCliPath: CARDANO_CLI_PATH,
    cardanoNodeConfigPath: CARDANO_NODE_CONFIG_PATH,
    cardanoNodeSocketPath: CARDANO_NODE_SOCKET_PATH,
    genesis: {
      byronPath: GENESIS_FILE_BYRON,
      shelleyPath: GENESIS_FILE_SHELLEY
    },
    hasuraCliPath: HASURA_CLI_PATH,
    hasuraUri: HASURA_URI,
    jqPath: JQ_PATH,
    listenAddress: LISTEN_ADDRESS,
    loggerMinSeverity: LOGGER_MIN_SEVERITY as LogLevelString,
    metadataServerUri: METADATA_SERVER_URI,
    pollingInterval: {
      adaSupply: Number(POLLING_INTERVAL_ADA_SUPPLY),
      metadataSync: {
        initial: Number(POLLING_INTERVAL_METADATA_SYNC_INITIAL),
        ongoing: Number(POLLING_INTERVAL_METADATA_SYNC_ONGOING)
      }
    },
    postgresDb: POSTGRES_DB,
    postgresDbFile: POSTGRES_DB_FILE,
    postgresHost: POSTGRES_HOST,
    postgresPassword: POSTGRES_PASSWORD,
    postgresPasswordFile: POSTGRES_PASSWORD_FILE,
    postgresPort: Number(POSTGRES_PORT),
    postgresUser: POSTGRES_USER,
    postgresUserFile: POSTGRES_USER_FILE,
    prometheusMetrics: PROMETHEUS_METRICS === 'true',
    queryDepthLimit: Number(QUERY_DEPTH_LIMIT),
    tracing: TRACING === 'true'
  }
}
