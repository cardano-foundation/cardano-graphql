import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const VRFVerificationKey = new GraphQLScalarType({
  name: 'VRFVerificationKey',
  description: 'Bech32 encoded VRF verification key',
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
  if (input.substring(0, 6) !== 'vrf_vk') {
    throw new GraphQLError(`${input} is not a valid VRF verification key`)
  }
  return input
}
