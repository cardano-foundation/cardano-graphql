import { ApolloClient, DocumentNode, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import { execSync } from 'child_process'
import { GraphQLSchema } from 'graphql'
import path from 'path'
import tmp from 'tmp-promise'
import util from '@cardano-graphql/util'
import { buildSchema as buildGenesisSchema } from '@cardano-graphql/api-genesis'
import { Server } from '@src/Server'

const byronTestnetGenesis = '../../../config/network/testnet/genesis/byron.json'
const shelleyTestnetGenesis = '../../../config/network/testnet/genesis/shelley.json'
const clientPath = path.resolve(__dirname, 'app_with_graphql_operations')
const port = 3301

describe('Server', () => {
  let client: ApolloClient<any>
  let allowedDocumentNode: DocumentNode
  let server: any
  let genesisSchema: GraphQLSchema

  beforeAll(async () => {
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
        uri: `http://localhost:${port}`,
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
        server = new Server([genesisSchema], { apiPort: port })
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
        const allowListPath = await tmp.tmpName({ postfix: '.json' })
        execSync(`npx persistgraphql ${clientPath} ${allowListPath}`)
        server = new Server(
          [genesisSchema],
          {
            allowListPath,
            apiPort: port
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
})
