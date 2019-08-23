import { MissingConfig } from './errors'
import { Config as ServerConfig } from './Server'

export function getConfig (): ServerConfig {
  const {
    apiPort,
    mockResponses,
    tracing,
    postgres
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || getPort(),
    mockResponses: process.env.NODE_ENV === 'production' ? false : mockResponses,
    postgres,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    MOCK_RESPONSES,
    TRACING,
    POSTGRES_DB,
    POSTGRES_HOST,
    POSTGRES_PASSWORD,
    POSTGRES_PORT,
    POSTGRES_USER
  } = env
  return {
    apiPort: Number(API_PORT),
    mockResponses: Boolean(MOCK_RESPONSES),
    postgres: {
      database: POSTGRES_DB ? String(POSTGRES_DB) : 'cexplorer',
      host: POSTGRES_HOST ? String(POSTGRES_HOST) : 'localhost',
      password: POSTGRES_PASSWORD ? String(POSTGRES_PASSWORD) : 'postgres',
      port: POSTGRES_PORT ? Number(POSTGRES_PORT) : 5432,
      username: POSTGRES_USER ? String(POSTGRES_USER) : 'nix'
    },
    tracing: Boolean(TRACING)
  }
}

function getPort () {
  // Dev-only
  if (process.env.NODE_ENV === 'production') throw new MissingConfig('API_PORT env not set')
  return 0
}
