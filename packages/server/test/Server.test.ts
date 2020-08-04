import { ApolloClient, DocumentNode, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import { execSync } from 'child_process'
import { GraphQLSchema, print } from 'graphql'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import path from 'path'
import tmp from 'tmp-promise'
import { buildSchema as buildGenesisSchema } from '@cardano-graphql/api-genesis'
import util from '@cardano-graphql/util'
import { Server } from '@src/Server'
import { IntrospectionNotPermitted, TracingRequired } from '@src/errors'

const byronTestnetGenesis = '../../../config/network/testnet/genesis/byron.json'
const shelleyTestnetGenesis = '../../../config/network/testnet/genesis/shelley.json'
const clientPath = path.resolve(__dirname, 'app_with_graphql_operations')
const apiPort = 3301
const apiUri = `http://localhost:${apiPort}`

describe('Server', () => {
  let client: ApolloClient<any>
  let allowedDocumentNode: DocumentNode
  let server: any
  let genesisSchema: GraphQLSchema
  let allowListPath: string

  beforeAll(async () => {
    allowListPath = await tmp.tmpName({ postfix: '.json' })
    execSync(`npx persistgraphql ${clientPath} ${allowListPath}`)
    genesisSchema = buildGenesisSchema({ byron: require(byronTestnetGenesis), shelley: require(shelleyTestnetGenesis) })
    allowedDocumentNode = await util.loadQueryNode(path.resolve(clientPath, 'src', 'feature_1'), 'maxLovelaceSupply')
  })

  beforeEach(async () => {
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
        uri: `http://localhost:${apiPort}`,
        fetch,
        fetchOptions: {
          fetchPolicy: 'no-cache'
        }
      })
    })
  })

  describe('Allowing specific queries', () => {
    describe('Booting the server without providing an allow-list', () => {
      beforeEach(async () => {
        server = new Server([genesisSchema], {
          allowIntrospection: false,
          apiPort,
          cacheEnabled: false,
          prometheusMetrics: false,
          tracing: false
        })
        await server.init()
        await server.start()
      })
      afterEach(() => {
        server.shutdown()
      })
      it('returns data for all valid queries', async () => {
        const validQueryResult = await client.query({
          query: gql`query valid {
              genesis {
                  shelley {
                      maxLovelaceSupply
                      systemStart
                  }
              }
          }`
        })
        expect(validQueryResult.data.genesis.shelley.maxLovelaceSupply).toBeDefined()
        expect(validQueryResult.errors).not.toBeDefined()
      })
    })

    describe('Providing an allow-list produced by persistgraphql, intended to limit the API for specific application requirements', () => {
      beforeEach(async () => {
        server = new Server(
          [genesisSchema],
          {
            allowListPath,
            allowIntrospection: false,
            apiPort,
            cacheEnabled: false,
            prometheusMetrics: false,
            tracing: false
          }
        )
        await server.init()
        await server.start()
      })
      afterEach(() => {
        server.shutdown()
      })

      it('Accepts allowed queries', async () => {
        const result = await client.query({
          query: allowedDocumentNode
        })
        expect(result.data.genesis.shelley.maxLovelaceSupply).toBeDefined()
        expect(result.errors).not.toBeDefined()
      })
      it('Returns a forbidden error and 403 HTTP response error if a valid but disallowed operation is sent', async () => {
        expect.assertions(2)
        try {
          await client.query({
            query: gql`query validButNotWhitelisted {
                genesis {
                    shelley {
                        maxLovelaceSupply
                        systemStart                        
                    }
                }
            }`
          })
        } catch (e) {
          expect(e.networkError.statusCode).toBe(403)
          expect(e.networkError.bodyText).toBe('Forbidden')
        }
      })
    })
  })

  describe('configuring introspection', () => {
    async function fetchSchemaViaIntrospection (): Promise<GraphQLSchema> {
      const executor = async ({ document, variables }: { document: DocumentNode, variables?: Object }) => {
        const query = print(document)
        const fetchResult = await fetch(apiUri, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ query, variables })
        })
        return fetchResult.json()
      }
      return wrapSchema({
        schema: await introspectSchema(executor),
        executor
      })
    }
    afterEach(() => {
      server.shutdown()
    })
    it('stops the schema being introspected', async () => {
      server = new Server([genesisSchema], {
        allowIntrospection: false,
        apiPort,
        cacheEnabled: false,
        prometheusMetrics: false,
        tracing: false
      })
      await server.init()
      await server.start()
      await expect(fetchSchemaViaIntrospection).rejects.toMatchObject({
        message: 'GraphQL introspection is not allowed by Apollo Server, but the query contained __schema or __type. To enable introspection, pass introspection: true to ApolloServer in production'
      })
    })
    it('allows the schema to be introspected', async () => {
      server = new Server([genesisSchema], {
        allowIntrospection: true,
        apiPort,
        cacheEnabled: false,
        prometheusMetrics: false,
        tracing: false
      })
      await server.init()
      await server.start()
      await expect(fetchSchemaViaIntrospection()).resolves.toBeInstanceOf(GraphQLSchema)
    })
    it('cannot be used with the allow-list feature enabled', async () => {
      server = new Server([genesisSchema], {
        allowIntrospection: true,
        allowListPath,
        apiPort,
        cacheEnabled: false,
        prometheusMetrics: false,
        tracing: false
      })
      await expect(server.init()).rejects.toBeInstanceOf(IntrospectionNotPermitted)
    })
  })

  describe('configuring metrics', () => {
    afterEach(() => {
      server.shutdown()
    })
    it('requires tracing to be enabled', async () => {
      server = new Server([genesisSchema], {
        allowIntrospection: false,
        apiPort,
        cacheEnabled: false,
        prometheusMetrics: true,
        tracing: true
      })
      await expect(server.init()).resolves
      server = new Server([genesisSchema], {
        allowIntrospection: false,
        apiPort,
        cacheEnabled: false,
        prometheusMetrics: true,
        tracing: false
      })
      await expect(server.init()).rejects.toBeInstanceOf(TracingRequired)
    })
  })
})
