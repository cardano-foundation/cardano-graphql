import { GraphQLScalarType, Kind } from 'graphql'

export const DateTime = new GraphQLScalarType({
  name: 'DateTime',
  description: 'SQL timestamp without timezone',
  serialize (value) {
    return value
  },
  parseValue (value) {
    return value
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return ast.value
    }
  }
})
