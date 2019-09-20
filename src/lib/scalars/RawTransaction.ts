import { GraphQLScalarType, Kind } from 'graphql'

export const RawTransaction = new GraphQLScalarType({
  name: 'RawTransaction',
  description: 'RawTransaction BLOB',
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
