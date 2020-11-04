import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const StakeAddress = new GraphQLScalarType({
  name: 'StakeAddress',
  description: 'Bech32 encoded stake address',
  serialize: validatePrefix,
  parseValue: validatePrefix,
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return validatePrefix(ast.value)
    }
  }
})

function validatePrefix (input: string) {
  if (input.substring(0, 5) !== 'stake') {
    throw new GraphQLError(`${input} is not a valid stake address`)
  }
  return input
}
