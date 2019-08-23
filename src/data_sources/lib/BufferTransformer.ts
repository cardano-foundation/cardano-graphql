import { FindOperator, FindOperatorType, ValueTransformer } from 'typeorm'

export class BufferTransformer implements ValueTransformer {
  to (value: string | FindOperator<string | string[]>) {
    if (!value) {
      return undefined
    } else if (typeof value === 'string') {
      return Buffer.from(value, 'hex')
    } else if (value instanceof FindOperator) {
      // Buffer is being used as part of a query e.g. In(...), LessThanOrEqual(...)
      const operator = value
      let newValue
      if (value.multipleParameters) {
        const arrayValue = (operator.value as string[]) || []
        newValue = arrayValue.map(v => Buffer.from(v, 'hex'))
      } else {
        newValue = value.value ? Buffer.from(value.value as string, 'hex') : undefined
      }
      // we need this naked reference so we can access the private _type property
      const operatorNaked = value as any
      return new FindOperator(
        operatorNaked._type as FindOperatorType,
        newValue,
        operator.useParameter,
        operator.multipleParameters
      )
    }
  }

  from (value: Buffer): string {
    return value.toString('hex')
  }
}
