import { LogLevelString } from 'bunyan'
import { cleanEnv, EnvError, host, makeValidator, port, Spec } from 'envalid'

// Todo: Hoist to util package, dedupe
// custom validator same as str from envalid but with typed return type
const str = <T extends string = string> (spec?: Spec<T>) => makeValidator((input: string) => {
  if (typeof input === 'string') return input as T
  throw new EnvError(`Not a string: "${input}"`)
})(spec)

export const getConfig = () => cleanEnv(process.env, {
  dgraphAddress: host(),
  loggerMinSeverity: str<LogLevelString>({ choices: ['trace', 'debug', 'info', 'warn', 'error', 'fatal'] }),
  ogmiosHost: host({ default: undefined }),
  ogmiosPort: port({ default: undefined })
})

export type Config = ReturnType<typeof getConfig>
