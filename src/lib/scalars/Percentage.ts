import { GraphQLScalarType, Kind } from 'graphql'

export const Percentage = new GraphQLScalarType({
  name: 'Percentage',
  description: '0-100',
  serialize (value) {
    // Todo: Implement
    return value
  },
  parseValue (value) {
    // Todo: Implement
    return value
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return parseInt(ast.value)
      case Kind.INT :
        return ast.value
    }
  }
})
