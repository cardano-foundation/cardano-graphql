import fs from 'fs'
import { ComplexityEstimatorArgs } from 'graphql-query-complexity'
import path from 'path'
import { makeExecutableSchema } from '@graphql-tools/schema'
import { Resolvers } from './graphql_types'

const fieldsComplexity = {
  Query: {
    extensionComplexityTest: {
      extensions: {
        complexity: ({ args, childComplexity }: ComplexityEstimatorArgs) =>
          args.multiplier * childComplexity
      }
    }
  },
  ExtensionComplexityTest: {
    // querying for a full ExtensionComplexityTest costs 36 points (with default = 1)
    fieldTwo: { extensions: { complexity: () => 10 } },
    fieldThree: { extensions: { complexity: () => 5 } },
    fieldFour: { extensions: { complexity: () => 20 } }
  }
}

export function buildSchema () {
  return makeExecutableSchema({
    resolvers: Object.assign({}, fieldsComplexity, {
      Query: {
        test: () => 'foo',
        testTwo: () => 'bar',
        extensionComplexityTest: {
          resolve: () => ({
            fieldOne: 'foo',
            fieldTwo: 23,
            fieldThree: true,
            fieldFour: 'bar'
          }),
          selectionSet: null,
          extensions: fieldsComplexity.Query.extensionComplexityTest.extensions
        },
        directiveComplexityTest: () => ({
          fieldOne: 'foo',
          fieldTwo: 23,
          fieldThree: true,
          fieldFour: 'bar'
        })
      }
    } as Resolvers),
    typeDefs: fs.readFileSync(
      path.resolve(__dirname, '..', 'schema.graphql'),
      'utf-8'
    )
  })
}
