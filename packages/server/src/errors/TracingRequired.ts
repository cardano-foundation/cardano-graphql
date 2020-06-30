import { CustomError } from 'ts-custom-error'

export class TracingRequired extends CustomError {
  public constructor (metricsType: string) {
    super()
    this.message = `Tracing is required for ${metricsType} metrics`
  }
}
