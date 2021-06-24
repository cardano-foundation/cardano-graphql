import { ApolloServerPlugin, GraphQLRequest } from 'apollo-server-plugin-base'
import { DocumentNode, GraphQLSchema, separateOperations } from 'graphql'
import { Logger } from 'ts-log'
import {
  simpleEstimator,
  fieldExtensionsEstimator,
  directiveEstimator,
  getComplexity
} from 'graphql-query-complexity'
import { QueryTooComplex } from '../errors'

const DEFAULT_FIELD_COMPLEXITY = 1

export const queryComplexityPlugin = (
  schema: GraphQLSchema,
  logger: Logger,
  maximumComplexity: number
): ApolloServerPlugin => ({
  requestDidStart: () => ({
    didResolveOperation ({
      request,
      document
    }: {
      request: GraphQLRequest;
      document: DocumentNode;
    }) {
      /**
       * This provides GraphQL query analysis to be able to react on complex queries to your GraphQL server.
       * This can be used to protect your GraphQL servers against resource exhaustion and DoS attacks.
       * More documentation can be found at https://github.com/ivome/graphql-query-complexity.
       */
      const complexity = getComplexity({
        // Our built schema
        schema,
        // To calculate query complexity properly,
        // we have to check if the document contains multiple operations
        // and eventually extract it operation from the whole query document.
        query: request.operationName
          ? separateOperations(document)[request.operationName]
          : document,
        // The variables for our GraphQL query
        variables: request.variables,
        // Complexity estimators
        estimators: [
          directiveEstimator({ name: 'complexity' }),
          fieldExtensionsEstimator(),
          simpleEstimator({ defaultComplexity: DEFAULT_FIELD_COMPLEXITY })
        ]
      })
      // Here we can react to the calculated complexity,
      // like compare it with max and throw error when the threshold is reached.
      if (complexity > maximumComplexity) {
        throw new QueryTooComplex(complexity, maximumComplexity)
      }
      // This can be used for logging or to implement rate limiting
      logger.trace('Used query complexity points:', complexity)
    }
  })
})
