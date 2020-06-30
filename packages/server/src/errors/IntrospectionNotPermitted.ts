import { CustomError } from 'ts-custom-error'

export class IntrospectionNotPermitted extends CustomError {
  public constructor (conflictingOption: string) {
    super()
    this.message = `Introspection is not permitted while ${conflictingOption} is enabled`
  }
}
