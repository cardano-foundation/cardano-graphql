import { GraphQLScalarType, Kind } from 'graphql'
import dayjs from 'dayjs'
import utc from 'dayjs/plugin/utc'
dayjs.extend(utc)

function isIsoDateString (string: string) {
  const isoDateRegExp = new RegExp(/(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+([+-][0-2]\d:[0-5]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d([+-][0-2]\d:[0-5]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d([+-][0-2]\d:[0-5]\d|Z))/)
  return isoDateRegExp.test(string)
}

export const DateTimeUtcToIso = new GraphQLScalarType({
  name: 'DateTime',
  description: 'DateTime ISO 8601',
  serialize (value) {
    if (isIsoDateString(value)) {
      return value
    }
    return dayjs.utc(value).format('YYYY-MM-DDTHH:mm:ss[Z]')
  },
  parseValue (value) {
    // Convert to UTC string
    if (!(typeof value === 'string')) {
      throw new TypeError(
        `DateTime cannot represent non string type ${JSON.stringify(value)}`
      )
    }

    if (!isIsoDateString(value)) {
      throw new TypeError(
        `DateTime cannot represent an invalid date-time-string ${value}.`
      )
    }
    return dayjs.utc(value).format('YYYY-MM-DDTHH:mm:ss')
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
    if (!isIsoDateString(value)) {
      throw new TypeError(
        `DateTime cannot represent an invalid date-time-string ${String(value)}.`
      )
    }
    return dayjs.utc(value).format('YYYY-MM-DDTHH:mm:ss')
  }
})
