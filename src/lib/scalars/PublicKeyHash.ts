import { GraphQLScalarType, Kind } from 'graphql'

export const PublicKeyHash = new GraphQLScalarType({
  name: 'PublicKeyHash',
  description: 'PublicKeyHash string',
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
    }
  }
})
