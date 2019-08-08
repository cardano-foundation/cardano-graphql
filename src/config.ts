import { MissingConfig } from './errors'

export function getConfig (): { apiPort: number, mockResponses: boolean } {
  const {
    apiPort,
    mockResponses
  } = filterAndTypecastEnvs(process.env)

  return {
    apiPort: apiPort || getPort(),
    mockResponses
  }
}

function filterAndTypecastEnvs (env: any) {
  const {
    API_PORT,
    MOCK_RESPONSES
  } = env
  return {
    apiPort: Number(API_PORT),
    mockResponses: Boolean(MOCK_RESPONSES)
  }
}

function getPort () {
  // Dev-only
  if (process.env.NODE_ENV === 'production') throw new MissingConfig('API_PORT env not set')
  return 0
}
