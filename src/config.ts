import { MissingConfig } from './errors'
import { Config as ServerConfig } from './Server'
import { BlockDataModel, TxDataModel, TxInDataModel, TxOutDataModel } from './data_sources/ledger/entities'
import { ConnectionManager } from 'typeorm'

export function getConfig (): ServerConfig {
  const connectionManager = new ConnectionManager()

  const {
    apiPort,
    tracing,
    postgres
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || getPort(),
    postgres: connectionManager.create({
      ...postgres,
      type: 'postgres',
      entities: [BlockDataModel, TxDataModel, TxInDataModel, TxOutDataModel]
    }),
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    TRACING,
    POSTGRES_DB,
    POSTGRES_HOST,
    POSTGRES_LOGGING,
    POSTGRES_PASSWORD,
    POSTGRES_PORT,
    POSTGRES_USER
  } = env
  return {
    apiPort: Number(API_PORT),
    postgres: {
      database: POSTGRES_DB ? String(POSTGRES_DB) : 'cexplorer',
      host: POSTGRES_HOST ? String(POSTGRES_HOST) : 'localhost',
      password: POSTGRES_PASSWORD ? String(POSTGRES_PASSWORD) : 'postgres',
      port: POSTGRES_PORT ? Number(POSTGRES_PORT) : 5432,
      username: POSTGRES_USER ? String(POSTGRES_USER) : 'nix',
      logging: POSTGRES_LOGGING ? Boolean(POSTGRES_LOGGING) : false
    },
    tracing: Boolean(TRACING)
  }
}

function getPort () {
  // Dev-only
  if (process.env.NODE_ENV === 'production') throw new MissingConfig('API_PORT env not set')
  return 0
}
