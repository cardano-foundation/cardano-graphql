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
  args: QueryArgs
): void {
  if (isEmpty(args.where)) {
    throw new GraphQLError('args.where cannot be empty')
  }
  Object.entries(args.where).forEach(([propertyName, exp]) => {
    if (isEmpty(exp)) {
      throw new GraphQLError(`Invalid expression ${JSON.stringify(exp)}. ${propertyName} cannot be empty`)
    }
  })
}
