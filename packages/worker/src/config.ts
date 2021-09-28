import { LogLevelString } from 'bunyan'
import { cleanEnv, EnvError, makeValidator, Spec } from 'envalid'

// custom validator same as str from envalid but with typed return type
const str = <T extends string = string> (spec?: Spec<T>) => makeValidator((input: string) => {
  if (typeof input === 'string') return input as T
  throw new EnvError(`Not a string: "${input}"`)
})(spec)

export const getConfig = () => cleanEnv(process.env, {
  loggerMinSeverity: str<LogLevelString>({ choices: ['trace', 'debug', 'info', 'warn', 'error', 'fatal'] })
})
