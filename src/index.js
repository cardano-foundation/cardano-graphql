import fs from 'fs'
import ON_DEATH from 'death'
import delay from 'delay'
import request from 'request-promise-native'
import cardanoSLGraphQLServer from './cardanoSLGraphQLServer'
import config from './config'

const {
  SWAGGER_SCHEMA_URI,
  REST_ENDPOINT,
  TLS_PATH_CA,
  TLS_PATH_KEY,
  TLS_PATH_CERT,
  PORT,
  CONNECTION_ATTEMPT_INTERVAL,
  MAX_CONNECTION_ATTEMPTS,
  AUTH_DISABLED,
  isProduction
} = config

let server

const exit = () => {
  server.shutdown(process.exit)
}

ON_DEATH((signal) => {
  switch (signal) {
    case 'SIGINT':
    case 'SIGQUIT':
    case 'SIGTERM':
      exit()
      break
    default: exit()
  }
})

const startServer = async (options) => {
  const requestOptions = {
    uri: SWAGGER_SCHEMA_URI.href
  }
  if (options.agentOptions) requestOptions.agentOptions = options.agentOptions
  const swaggerSchema = JSON.parse(await request(requestOptions))
  server = await cardanoSLGraphQLServer(swaggerSchema, REST_ENDPOINT, options)
  server.listen({ port: PORT }, () => {
    console.log(`🚀 Server ready at ${server.endpoint(PORT)}`)
  })
  return server
}

const hasTlsAuth = () => new Promise((resolve, reject) => {
  fs.access(TLS_PATH_CA, (error) => {
    if (error && error.code === 'ENOENT') {
      return resolve(false)
    } else if (error) {
      return reject(error)
    }
    resolve(true)
  })
})

const apiAvailable = (options) => new Promise((resolve, reject) => {
  const requestOptions = {
    uri: SWAGGER_SCHEMA_URI,
    resolveWithFullResponse: true
  }
  if (options.agentOptions) requestOptions.agentOptions = options.agentOptions
  request(requestOptions).then(({ statusCode }) => resolve(statusCode === 200)).catch((error) => {
    if (error.cause.code === 'ECONNREFUSED') return resolve(false)
    reject(error)
  })
})

let attemptCount = 0
export const initialize = async () => {
  // Wait until Wallet REST API certificate is provisioned
  if (attemptCount >= MAX_CONNECTION_ATTEMPTS) {
    throw new Error(`Can not connect to Wallet API after ${attemptCount} attempts`)
  }
  attemptCount++
  try {
    const options = {}
    if (!AUTH_DISABLED && await hasTlsAuth()) {
      options.agentOptions = {
        ca: fs.readFileSync(TLS_PATH_CA),
        key: fs.readFileSync(TLS_PATH_KEY),
        cert: fs.readFileSync(TLS_PATH_CERT),
        rejectUnauthorized: isProduction
      }
    } else if (AUTH_DISABLED) {
      options.agentOptions = {
        rejectUnauthorized: isProduction
      }
    }
    if (await apiAvailable(options)) return startServer(options)
    await delay(CONNECTION_ATTEMPT_INTERVAL)
    return initialize()
  } catch (error) {
    console.error(error.message)
    throw error
  }
}

initialize()
