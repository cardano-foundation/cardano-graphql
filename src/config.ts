import { MissingConfig } from './errors'

export function getConfig (): { apiPort: number, dbUri: string } {
  const {
    apiPort,
    dbUri,
  } = filterAndTypecastEnvs(process.env)

  if (!apiPort) throw new MissingConfig('API_PORT env not set')
  return {
    apiPort,
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
