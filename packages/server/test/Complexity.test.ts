import { ApolloClient, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import { GraphQLSchema } from 'graphql'
import path from 'path'
import util from '@cardano-graphql/util'
import { Server } from '@src/Server'
import { buildSchema } from './api/src'
import {
  directiveEstimator,
  fieldExtensionsEstimator,
  getComplexity,
  simpleEstimator
} from 'graphql-query-complexity'

const clientPath = path.resolve(__dirname, 'app_with_graphql_operations')
const staticConfig = {
  apiPort: 3301,
  listenAddress: '127.0.0.1',
  maxQueryComplexity: 1000
}

describe('Query Complexity', () => {
  let client: ApolloClient<any>
  let server: any
  let schema: GraphQLSchema

  beforeAll(async () => {
    schema = buildSchema()
    server = new Server([schema], {
      ...staticConfig,
      allowIntrospection: false,
      cacheEnabled: false,
      prometheusMetrics: false,
      tracing: false
    })
    client = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false
      }),
      defaultOptions: {
        query: {
          fetchPolicy: 'network-only'
        }
      },
      link: createHttpLink({
        uri: `http://${staticConfig.listenAddress}:${staticConfig.apiPort}`,
        fetch,
        fetchOptions: {
          fetchPolicy: 'no-cache'
        }
      })
    })
    await server.init()
  })

  beforeEach(async () => {
    await server.start()
  })

  afterEach(() => {
    server.shutdown()
  })

  describe('Field extensions estimator', () => {
    it(
      'should have a complexity of a full ExtensionComplexityTest' +
        ' plus extensionComplexityTest cost without args',
      async () => {
        const query = await util.loadQueryNode(
          path.resolve(clientPath, 'src', 'query_complexity'),
          'field_extension_test'
        )
        const queryResult = await client.query({
          query
        })
        const computedComplexity = getComplexity({
          schema,
          estimators: [
            directiveEstimator(),
            fieldExtensionsEstimator(),
            simpleEstimator({ defaultComplexity: 1 })
          ],
          query
        })
        // ExtensionComplexityTest (36) + extensionComplexityTest without args (1)
        expect(computedComplexity).toEqual(37)
        expect(queryResult.data.extensionComplexityTest).toBeDefined()
        expect(queryResult.errors).not.toBeDefined()
      }
    )
    it(
      'should have a complexity of a full ExtensionComplexityTest' +
        ' multiplied by the extensionComplexityTest arg value',
      async () => {
        const query = await util.loadQueryNode(
          path.resolve(clientPath, 'src', 'query_complexity'),
          'field_extension_test'
        )
        const toExecute = {
          query,
          variables: { multiplier: 10 }
        }
        const queryResult = await client.query(toExecute)
        const computedComplexity = getComplexity({
          schema,
          estimators: [
            directiveEstimator(),
            fieldExtensionsEstimator(),
            simpleEstimator({ defaultComplexity: 1 })
          ],
          ...toExecute
        })
        // ExtensionComplexityTest (36) * extensionComplexityTest args (10)
        expect(computedComplexity).toEqual(360)
        expect(queryResult.data.extensionComplexityTest).toBeDefined()
        expect(queryResult.errors).not.toBeDefined()
      }
    )
    it('should throw an error when the complexity is greater than the max allowed', async () => {
      const query = await util.loadQueryNode(
        path.resolve(clientPath, 'src', 'query_complexity'),
        'field_extension_test'
      )
      const toExecute = {
        query,
        variables: { multiplier: 100 }
      }
      await expect(client.query(toExecute)).rejects.toThrow()
      const computedComplexity = getComplexity({
        schema,
        estimators: [
          directiveEstimator(),
          fieldExtensionsEstimator(),
          simpleEstimator({ defaultComplexity: 1 })
        ],
        ...toExecute
      })
      expect(computedComplexity).toEqual(3600)
    })
  })

  describe('Directive estimator', () => {
    it(
      'should have a complexity of a full DirectiveComplexityTest' +
        ' plus directiveComplexityTest cost without args',
      async () => {
        const query = await util.loadQueryNode(
          path.resolve(clientPath, 'src', 'query_complexity'),
          'directive_test'
        )
        const queryResult = await client.query({
          query
        })
        const computedComplexity = getComplexity({
          schema,
          estimators: [
            directiveEstimator(),
            fieldExtensionsEstimator(),
            simpleEstimator({ defaultComplexity: 1 })
          ],
          query
        })
        // DirectiveComplexityTest (31) + directiveComplexityTest without args (1)
        expect(computedComplexity).toEqual(32)
        expect(queryResult.data.directiveComplexityTest).toBeDefined()
        expect(queryResult.errors).not.toBeDefined()
      }
    )
    it(
      'should have a complexity of a full DirectiveComplexityTest' +
        ' multiplied by the directiveComplexityTest arg value',
      async () => {
        const query = await util.loadQueryNode(
          path.resolve(clientPath, 'src', 'query_complexity'),
          'directive_test'
        )
        const toExecute = {
          query,
          variables: { multiplier: 10 }
        }
        const queryResult = await client.query(toExecute)
        const computedComplexity = getComplexity({
          schema,
          estimators: [
            directiveEstimator(),
            fieldExtensionsEstimator(),
            simpleEstimator({ defaultComplexity: 1 })
          ],
          ...toExecute
        })
        // (DirectiveComplexityTest (31) + directiveComplexityTest value (1)) * directiveComplexityTest multipliers (10)
        expect(computedComplexity).toEqual(320)
        expect(queryResult.data.directiveComplexityTest).toBeDefined()
        expect(queryResult.errors).not.toBeDefined()
      }
    )
    it('should throw an error when the complexity is greater than the max allowed', async () => {
      const query = await util.loadQueryNode(
        path.resolve(clientPath, 'src', 'query_complexity'),
        'directive_test'
      )
      const toExecute = {
        query,
        variables: { multiplier: 100 }
      }
      await expect(client.query(toExecute)).rejects.toThrow()
      const computedComplexity = getComplexity({
        schema,
        estimators: [
          directiveEstimator(),
          fieldExtensionsEstimator(),
          simpleEstimator({ defaultComplexity: 1 })
        ],
        ...toExecute
      })
      expect(computedComplexity).toEqual(3200)
    })
  })
})
