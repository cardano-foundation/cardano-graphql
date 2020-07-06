import { GraphQLScalarType, Kind } from 'graphql'
import { DateTimeResolver } from 'graphql-scalars'
import { validateDateTime } from '../dateTimeValidation'

function isoToUtc (isoString: string) {
  return isoString.substring(0, isoString.length - 5)
}

export const DateTimeUtcToIso = new GraphQLScalarType({
  name: 'DateTime',
  description: 'UTC DateTime to ISO RFC 3339',
  serialize (value) {
    return DateTimeResolver.serialize.call(this, `${value}Z`)
  },
  parseValue (value) {
    // Trim .000Z
    if (!(typeof value === 'string')) {
      throw new TypeError(
        `DateTime cannot represent non string type ${JSON.stringify(value)}`
      )
    }

    if (!validateDateTime(value)) {
      throw new TypeError(
        `DateTime cannot represent an invalid date-time-string ${value}.`
      )
    }
    return isoToUtc(value)
  },
  parseLiteral (ast) {
    if (ast.kind !== Kind.STRING) {
      throw new TypeError(
        `DateTime cannot represent non string type ${
          'value' in ast && ast.value
        }`
      )
    }
    const { value } = ast
    if (!validateDateTime(value)) {
      throw new TypeError(
        `DateTime cannot represent an invalid date-time-string ${String(value)}.`
      )
    }
    return isoToUtc(value)
  }
})
