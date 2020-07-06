import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const Hash32HexString = new GraphQLScalarType({
  name: 'Hash32HexString',
  description: 'Hex encoded hash32 string, 64 characters',
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
  if (input.length !== 64) throw new GraphQLError(`${input} is not a valid hash`)
  return `\\x${input}`
}
