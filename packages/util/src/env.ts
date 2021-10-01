import { EnvError, makeValidator, Spec } from 'envalid'

/**
 * custom validator same as str from envalid but with typed return type
 * @param spec An object that specifies the format of required var.
 */
export const str = <T extends string = string> (spec?: Spec<T>) => makeValidator((input: string) => {
  if (typeof input === 'string') return input as T
  throw new EnvError(`Not a string: "${input}"`)
})(spec)
