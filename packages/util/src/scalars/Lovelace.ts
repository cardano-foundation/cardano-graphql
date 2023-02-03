import { GraphQLScalarType, Kind } from 'graphql'

export const Lovelace = new GraphQLScalarType({
  name: 'Lovelace',
  description: 'The atomic unit of Ada, the principle asset of Cardano',
  serialize (value: string) {
    return value
  },
  parseValue (value: string) {
    return value
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return ast.value
    }
  }
})
