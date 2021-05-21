import { Logger } from 'ts-log'
import queryComplexity, {
  directiveEstimator,
  fieldExtensionsEstimator,
  simpleEstimator
} from 'graphql-query-complexity'
import { QueryTooComplex } from '../errors'

const DEFAULT_FIELD_COMPLEXITY = 1

export function queryComplexityValidator (
  logger: Logger,
  maximumComplexity: number
) {
  return queryComplexity({
    // The maximum allowed query complexity, queries above this threshold will be rejected
    maximumComplexity,
    // The query variables. This is needed because the variables are not available
    // in the visitor of the graphql-js library
    variables: {},
    // Optional callback function to retrieve the determined query complexity
    // Will be invoked whether the query is rejected or not
    // This can be used for logging or to implement rate limiting
    onComplete: (complexity: number) => {
      logger.info('Determined query complexity: ', complexity)
    },
    // Optional function to create a custom error
    createError: (max: number, actual: number) => {
      return new QueryTooComplex(actual, max)
    },
    // Add any number of estimators. The estimators are invoked in order, the first
    // numeric value that is being returned by an estimator is used as the field complexity.
    // If no estimator returns a value, an exception is raised.
    estimators: [
      directiveEstimator({ name: 'complexity' }),
      fieldExtensionsEstimator(),
      simpleEstimator({ defaultComplexity: DEFAULT_FIELD_COMPLEXITY })
    ]
  })
}
