import { GraphQLScalarType, Kind } from 'graphql'

export const Lovelaces = new GraphQLScalarType({
  name: 'LoveLaces',
  description: 'LoveLaces, the atomic unit of ADA',
  serialize (value) {
    return value
  },
  parseValue (value) {
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
