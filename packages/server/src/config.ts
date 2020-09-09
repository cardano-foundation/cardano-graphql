import { Config as ApiCardanoDbHasuraConfig } from '@cardano-graphql/api-cardano-db-hasura'
import { Config as ApiGenesisConfig } from '@cardano-graphql/api-genesis'
import { MissingConfig } from './errors'
import fs from 'fs-extra'
import { Config as ServerConfig } from './Server'

export type Config = ServerConfig & ApiCardanoDbHasuraConfig & ApiGenesisConfig

export async function getConfig (): Promise<Config> {
  const {
    allowIntrospection,
    allowedOrigins,
    allowListPath,
    apiPort,
    cacheEnabled,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    listenAddress,
    postgresDb,
    postgresDbFile,
    postgresHost,
    postgresPassword,
    postgresPasswordFile,
    postgresPort,
    postgresUser,
    postgresUserFile,
    prometheusMetrics,
    queryDepthLimit,
    tracing
  } = filterAndTypecastEnvs(process.env)

  if (!hasuraUri && !genesisFileShelley) {
    throw new MissingConfig(
      `You have not provided configuration to load an API segment. Either set HASURA_URI or 
      GENESIS_FILE_SHELLEY and GENESIS_FILE_SHELLEY`
    )
  }
  if (!postgresDbFile && !postgresDb) {
    throw new MissingConfig('POSTGRES_DB_FILE or POSTGRES_DB env not set')
  }
  if (!postgresHost) {
    throw new MissingConfig('POSTGRES_HOST env not set')
  }
  if (!postgresPasswordFile && !postgresPassword) {
    throw new MissingConfig('POSTGRES_PASSWORD_FILE or POSTGRES_PASSWORD env not set')
  }
  if (!postgresPort) {
    throw new MissingConfig('POSTGRES_PORT env not set')
  }
  if (!postgresUserFile && !postgresUser) {
    throw new MissingConfig('POSTGRES_USER_FILE or POSTGRES_USER env not set')
  }
  let db: Config['db']
  try {
    db = {
      database: postgresDb || (await fs.readFile(postgresDbFile, 'utf8')).toString(),
      host: postgresHost,
      password: postgresPassword || (await fs.readFile(postgresPasswordFile, 'utf8')).toString(),
      port: postgresPort,
      user: postgresUser || (await fs.readFile(postgresUserFile, 'utf8')).toString()
    }
  } catch (error) {
    throw new MissingConfig('Database configuration cannot be read')
  }
  return {
    allowIntrospection:
      (process.env.NODE_ENV === 'production' && allowIntrospection) ||
      (process.env.NODE_ENV !== 'production' && allowIntrospection !== false),
    allowedOrigins: allowedOrigins || true,
    allowListPath,
    apiPort: apiPort || 3100,
    cacheEnabled: cacheEnabled || false,
    db,
    genesisFileByron,
    genesisFileShelley,
    hasuraUri,
    listenAddress: listenAddress || '0.0.0.0',
    prometheusMetrics,
    queryDepthLimit: queryDepthLimit || 10,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    ALLOW_INTROSPECTION,
    ALLOWED_ORIGINS,
    ALLOW_LIST_PATH,
    API_PORT,
    CACHE_ENABLED,
    GENESIS_FILE_BYRON,
    GENESIS_FILE_SHELLEY,
    HASURA_URI,
    LISTEN_ADDRESS,
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
    TRACING,
    WHITELIST_PATH
  } = env
  if (WHITELIST_PATH) {
    console.warn(`WHITELIST_PATH is deprecated and will be removed in 3.0.0.
    Please update your configuration to use ALLOW_LIST_PATH instead.`)
  }
  return {
    allowIntrospection: ALLOW_INTROSPECTION === 'true',
    allowedOrigins: ALLOWED_ORIGINS,
    allowListPath: ALLOW_LIST_PATH || WHITELIST_PATH,
    apiPort: Number(API_PORT),
    cacheEnabled: CACHE_ENABLED === 'true',
    genesisFileByron: GENESIS_FILE_BYRON,
    genesisFileShelley: GENESIS_FILE_SHELLEY,
    hasuraUri: HASURA_URI,
    listenAddress: LISTEN_ADDRESS,
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
