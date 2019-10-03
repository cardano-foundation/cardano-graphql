import { GraphQLScalarType, Kind } from 'graphql'

export const TransactionHash = new GraphQLScalarType({
  name: 'TransactionHash',
  description: 'TransactionHash string',
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
