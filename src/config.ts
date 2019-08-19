import { MissingConfig } from './errors'

export function getConfig (): {
  apiPort: number,
  mockResponses: boolean,
  tracing: boolean
  } {
  const {
    apiPort,
    mockResponses,
    tracing
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || getPort(),
    mockResponses: process.env.NODE_ENV === 'production' ? false : mockResponses,
    tracing
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    MOCK_RESPONSES,
    TRACING
  } = env
  return {
    apiPort: Number(API_PORT),
    mockResponses: Boolean(MOCK_RESPONSES),
    tracing: Boolean(TRACING)
  }
}

function getPort () {
  // Dev-only
  if (process.env.NODE_ENV === 'production') throw new MissingConfig('API_PORT env not set')
  return 0
}
