import { CustomError } from 'ts-custom-error'

export class MissingConfig extends CustomError {
  public constructor (message: string) {
    super()
    this.message = message
  }
}
