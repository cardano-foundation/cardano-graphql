import { GraphQLScalarType, Kind } from 'graphql'

export const Hex = new GraphQLScalarType({
  name: 'Hex',
  description: 'Hex encoded bytes',
  serialize (value: string) {
    return value.substring(2)
  },
  parseValue (value: string) {
    return validateInput(value)
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return validateInput(ast.value)
    }
  }
})

function validateInput (input: string) {
  return `\\x${input}`
}
