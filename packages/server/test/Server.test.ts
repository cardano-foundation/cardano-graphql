import { ApolloClient, DocumentNode, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import { execSync } from 'child_process'
import express, { Application } from 'express'
import { GraphQLSchema } from 'graphql'
import fs from 'fs'
import http from 'http'
import path from 'path'
import tmp from 'tmp-promise'
import util from '@cardano-graphql/util'
import { buildSchema as buildGenesisSchema } from '@cardano-graphql/api-genesis'
import { Server } from '@src/Server'
import { whitelistPlugin } from '@src/apollo_server_plugins'

const shelleyTestnetGenesis = '../../../config/network/shelley_testnet/genesis_shelley.json'
const clientPath = path.resolve(__dirname, 'app_with_graphql_operations')
const port = 3101

function listen (app: Application, port: number): Promise<http.Server> {
  return new Promise(function (resolve, reject) {
    const server: http.Server = app.listen(port, () => resolve(server))
    server.on('error', reject)
  })
}

describe('Server', () => {
  let client: ApolloClient<any>
  let whiteListedDocumentNode: DocumentNode
  let app: express.Application
  let httpServer: http.Server
  let genesisSchema: GraphQLSchema

  beforeAll(async () => {
    genesisSchema = buildGenesisSchema({ shelley: require(shelleyTestnetGenesis) })
    whiteListedDocumentNode = await util.loadQueryNode(path.resolve(clientPath, 'src', 'feature_1'), 'maxLovelaceSupply')
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
    app = express()
  })

  describe('Whitelisting', () => {
    describe('Booting the server without providing a whitelist', () => {
      beforeEach(async () => {
        Server(app, {
          schema: genesisSchema
        })
        httpServer = await listen(app, port)
      })
      afterEach(() => {
        httpServer.close()
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

    describe('Providing a whitelist produced by persistgraphql, intended to limit the API for specific application requirements', () => {
      beforeEach(async () => {
        const whitelistPath = await tmp.tmpName({ postfix: '.json' })
        execSync(`npx persistgraphql ${clientPath} ${whitelistPath}`)
        const whitelist = JSON.parse(fs.readFileSync(whitelistPath, 'utf-8'))
        Server(app, {
          plugins: [whitelistPlugin(whitelist)],
          schema: genesisSchema
        })
        httpServer = await listen(app, port)
      })
      afterEach(() => {
        httpServer.close()
      })

      it('Accepts listed queries', async () => {
        const result = await client.query({
          query: whiteListedDocumentNode
        })
        expect(result.data.genesis.shelley.maxLovelaceSupply).toBeDefined()
        expect(result.errors).not.toBeDefined()
      })
      it('Returns a networkError if a valid but unlisted operation is sent', async () => {
        expect.assertions(1)
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
          expect(e.networkError.result.errors[0].message).toBe('Operation is forbidden')
        }
      })
    })
  })
})
