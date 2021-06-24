import { CustomError } from 'ts-custom-error'

export class OperationRequiresNodeInCurrentEra extends CustomError {
  public constructor (operation: string) {
    super()
    this.message = `${operation} cannot be called until cardano-node's block sync is in the current era`
  }
}
