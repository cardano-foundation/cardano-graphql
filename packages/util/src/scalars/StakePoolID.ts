import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const StakePoolID = new GraphQLScalarType({
  name: 'StakePoolID',
  description: 'Bech32 encoded Stake Pool ID',
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
  if (input.substring(0, 4) !== 'pool') {
    throw new GraphQLError(`${input} is not a valid Stake Pool ID`)
  }
  return input
}
