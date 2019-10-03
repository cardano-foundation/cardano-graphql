import { GraphQLError } from 'graphql'

export function checkLimit (requested: number, maxAllowed: number): void {
  if (requested < 0) throw new GraphQLError('Limit must be a positive integer')
  if (requested > maxAllowed) {
    throw new GraphQLError(
      `${requested} exceeds the maximum allowed value of ${maxAllowed}. Use the offset to paginate through a larger result set`
    )
  }
}
