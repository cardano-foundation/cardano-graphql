import { GraphQLError } from 'graphql'
import {
  QueryBlocksArgs,
  QueryEpochsArgs,
  QueryTransactionsArgs,
  QueryUtxoSetArgs
} from '../graphql_types'
const isEmpty = require('lodash.isempty')

export type QueryArgs = QueryBlocksArgs |
  QueryEpochsArgs |
  QueryTransactionsArgs |
  QueryUtxoSetArgs

export function validateQueryArgs (
  args: QueryArgs,
  maxAllowed: number
): void {
  const { where, limit: requested } = args
  if (isEmpty(where)) {
    throw new GraphQLError('args.where cannot be empty')
  }
  Object.entries(where).forEach(([ propertyName, exp ]) => {
    if (isEmpty(exp)) {
      throw new GraphQLError(`Invalid expression ${JSON.stringify(exp)}. ${propertyName} cannot be empty`)
    }
  })
  if (requested < 0) throw new GraphQLError('Limit must be a positive integer')
  if (requested > maxAllowed) {
    throw new GraphQLError(
      `${requested} exceeds the maximum allowed value of ${maxAllowed}. Use the offset to paginate through a larger result set`
    )
  }
}
