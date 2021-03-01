import { GraphQLError, GraphQLScalarType, Kind } from 'graphql'

export const AssetFingerprint = new GraphQLScalarType({
  name: 'AssetFingerprint',
  description: 'CIP14 User-facing asset fingerprint as a bech32-encoded blake2b-160 digest of the concatenation of policy id and asset name.',
  serialize (value: string) {
    return value
  },
  parseValue (value: string) {
    return validateInput(value)
  },
  parseLiteral (ast) {
    switch (ast.kind) {
      case Kind.STRING :
        return validateInput(ast.value)
    }
  }
})

function validateInput (input: string) {
  if (
    input.length !== 44 &&
    input.substr(0, 5) !== 'asset'
  ) throw new GraphQLError(`${input} is not a valid AssetFingerprint`)
  return input
}
