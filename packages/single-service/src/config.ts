import { LogLevelString } from 'bunyan'
import { cleanEnv, host, port } from 'envalid'
import { str } from '@cardano-graphql/util'

export const getConfig = () => {
  const envs = cleanEnv(process.env, {
    LOGGER_MIN_SEVERITY: str<LogLevelString>({ choices: ['trace', 'debug', 'info', 'warn', 'error', 'fatal'] }),
    OGMIOS_HOST: host(),
    OGMIOS_PORT: port()
  })

  return {
    loggerMinSeverity: envs.LOGGER_MIN_SEVERITY,
    ogmios: {
      host: envs.OGMIOS_HOST,
      port: envs.OGMIOS_PORT
    }
  }
}
