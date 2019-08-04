import { MissingConfig } from './errors'

export function getConfig (): { apiPort: number, dbUri: string } {
  const {
    apiPort,
    dbUri,
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || randomPort(),
    dbUri
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    DB_URI,
  } = env
  return {
    apiPort: Number(API_PORT),
    dbUri: DB_URI
  }
}

function randomPort() {
  // Dev-only
  if (process.env.NODE_ENV === 'production') throw new MissingConfig('API_PORT env not set')
  return 0
}