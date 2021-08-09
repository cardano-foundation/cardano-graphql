import { CustomError } from 'ts-custom-error'

export class OperationRequiresSyncedNode extends CustomError {
  public constructor (operation: string) {
    super()
    this.message = `Cannot ${operation} until cardano-node is close to the network tip`
  }
}
