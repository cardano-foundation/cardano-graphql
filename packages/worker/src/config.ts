import { LogLevelString } from 'bunyan'
import { cleanEnv } from 'envalid'
import { str } from '@cardano-graphql/util'

export const getConfig = () => {
  const envs = cleanEnv(process.env, {
    LOGGER_MIN_SEVERITY: str<LogLevelString>({ choices: ['trace', 'debug', 'info', 'warn', 'error', 'fatal'] })
  })

  return {
    loggerMinSeverity: envs.LOGGER_MIN_SEVERITY
  }
}
