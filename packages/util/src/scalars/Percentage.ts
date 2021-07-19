import { GraphQLScalarType, Kind } from 'graphql'

const REGEX = /(^100(\.0{1,2})?$)|(^([1-9]([0-9])?|0)(\.[0-9]{1,2})?$)/g
export const INVALID_REGEX_ERROR = new TypeError(`Percentage requires Regex: ${REGEX}`);

function roundFloatFromString (value: string) {
  return parseFloat(parseFloat(value).toFixed(2))
}

export const Percentage = new GraphQLScalarType({
  name: 'Percentage',
  description: '0-100.00',
  serialize (value) {
    const rounded = roundFloatFromString(value)
    if (rounded > 100 || rounded < 0) {
      throw new TypeError('Percentage must be a number between 0 - 100.00')
    }
    return rounded
  },
  parseValue (value) {
    if (typeof value === 'string') {
      if (!REGEX.test(value)) {
        throw INVALID_REGEX_ERROR;
      }
      return roundFloatFromString(value)
    }
    return parseFloat(value).toFixed(2)
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        if (!REGEX.test(ast.value)) {
          throw INVALID_REGEX_ERROR;
        }
        return roundFloatFromString(ast.value)
      case Kind.FLOAT :
        return parseFloat(ast.value).toFixed(2)
      case Kind.INT :
        return parseFloat(ast.value).toFixed(2)
    }
  }
})
