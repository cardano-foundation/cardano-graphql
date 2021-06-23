import { CustomError } from 'ts-custom-error'

export class ModuleIsNotInitialized extends CustomError {
  public constructor (moduleName: string, methodName: string) {
    super()
    this.message = `${methodName} cannot be called until ${moduleName} is initialized`
  }
}
