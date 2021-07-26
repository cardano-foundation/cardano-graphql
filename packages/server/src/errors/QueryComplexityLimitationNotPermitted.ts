import { CustomError } from 'ts-custom-error'

export class QueryComplexityLimitationNotPermitted extends CustomError {
  public constructor (conflictingOption: string) {
    super()
    this.message = `Query complexity limitation is not permitted while ${conflictingOption} is enabled`
  }
}
