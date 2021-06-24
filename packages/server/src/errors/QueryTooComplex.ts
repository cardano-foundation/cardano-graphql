import { GraphQLError } from 'graphql'

export class QueryTooComplex extends GraphQLError {
  public constructor (actual: number, maxAllowed: number) {
    super(
      `Query is too complex: ${actual}. Maximum allowed complexity: ${maxAllowed}`
    )
  }
}
