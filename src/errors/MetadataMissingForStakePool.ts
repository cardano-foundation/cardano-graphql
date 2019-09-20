import { CustomError } from 'ts-custom-error'

export class MetadataMissingForStakePool extends CustomError {
  public constructor (id: string) {
    super()
    this.message = `Stakepool ${id} has not registered metadata`
  }
}

