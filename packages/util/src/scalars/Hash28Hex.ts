import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const Hash28Hex = new GraphQLScalarType({
  name: 'Hash28Hex',
  description: 'Hex encoded 28 byte hash, 56 characters',
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
  if (input.length !== 56) throw new GraphQLError(`${input} is not a valid 28 byte hash`)
  return `\\x${input}`
}
