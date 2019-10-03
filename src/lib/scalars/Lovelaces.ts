import { GraphQLScalarType, Kind } from 'graphql'

export const LoveLaces = new GraphQLScalarType({
  name: 'LoveLaces',
  description: 'LoveLaces, the atomic unit of ADA',
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
