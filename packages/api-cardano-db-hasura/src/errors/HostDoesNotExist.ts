import { CustomError } from 'ts-custom-error'

export class HostDoesNotExist extends CustomError {
  public constructor (service: string) {
    super()
    this.message = `Host for ${service} does not exist`
  }
}
