import { GraphQLScalarType, Kind } from 'graphql'

export const StakePoolTicker = new GraphQLScalarType({
  name: 'StakePoolTicker',
  description: 'Unique ticker code for a stake pool, up to 4 characters',
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
