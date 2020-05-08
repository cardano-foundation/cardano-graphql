import { ApolloClient, DocumentNode, gql, InMemoryCache } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import { execSync } from 'child_process'
import * as express from 'express'
import { Application } from 'express'
import * as fs from 'fs'
import * as http from 'http'
import * as path from 'path'
import * as tmp from 'tmp-promise'
import { Server } from '../Server'
import resolvers from '../resolvers'
import { buildContext, Context } from '../Context'
import { whitelistPlugin } from '../apollo_server_plugins'
import { loadDocumentNode } from '../lib/loadDocumentNode'

const clientPath = path.resolve(__dirname, 'app_with_graphql_operations')
const schemaPath = path.resolve(__dirname, '..', 'schema.graphql')
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
  let context: Context

  beforeAll(async () => {
    context = await buildContext('http://localhost:8090')
    whiteListedDocumentNode = await loadDocumentNode(path.resolve(clientPath, 'src', 'feature_1', 'cardanoDynamic.graphql'))
  })

  beforeEach(async () => {
    client = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false
      }),
      link: createHttpLink({
        uri: `http://localhost:${port}`,
        fetch
      })
    })
    app = express()
  })

  describe('Whitelisting', () => {
    describe('Booting the server without providing a whitelist', () => {
      beforeEach(async () => {
        Server(app, {
          context,
          resolvers,
          typeDefs: fs.readFileSync(schemaPath, 'UTF8')
        })
        httpServer = await listen(app, port)
      })
      afterEach(() => {
        httpServer.close()
      })
      it('returns data for all valid queries', async () => {
        const validQueryResult = await client.query({
          query: gql`query validButNotWhitelisted {
              cardano {
                  networkName
              }
          }`
        })
        expect(validQueryResult.data.cardano.networkName).toBeDefined()
        expect(validQueryResult.errors).not.toBeDefined()
      })
    })

    describe('Providing a whitelist produced by persistgraphql, intended to limit the API for specific application requirements', () => {
      beforeEach(async () => {
        const whitelistPath = await tmp.tmpName({ postfix: '.json' })
        execSync(`npx persistgraphql ${clientPath} ${whitelistPath}`)
        const whitelist = JSON.parse(fs.readFileSync(whitelistPath, 'UTF8'))
        Server(app, {
          context,
          plugins: [whitelistPlugin(whitelist)],
          resolvers,
          typeDefs: fs.readFileSync(schemaPath, 'UTF8')
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
        expect(result.data.cardano.blockHeight).toBeDefined()
        expect(result.errors).not.toBeDefined()
      })
      it('Returns a networkError if a valid but unlisted operation is sent', async () => {
        expect.assertions(1)
        try {
          await client.query({
            query: gql`query validButNotWhitelisted {
                cardano {
                    networkName
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
