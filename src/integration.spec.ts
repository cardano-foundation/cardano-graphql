import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import { Context } from './Context'
import { Ledger, TxDataModel } from './data_sources'
import { getConfig } from './config'
import { resolvers } from './resolvers'

const { postgres } = getConfig()

describe('Integration', () => {
  let apolloServer: ApolloServerBase
  let client: ApolloServerTestClient

  beforeEach(async () => {
    await postgres.connect()
    apolloServer = new ApolloServerBase({
      dataSources (): Context['dataSources'] {
        return {
          ledger: new Ledger({
            transactions: postgres.getRepository(TxDataModel)
          })
        }
      },
      introspection: true,
      resolvers,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
    })
    client = createTestClient(apolloServer)
  })

  afterEach(() => postgres.close())

  describe('Ledger', () => {
    describe('transactions', () => {
      it('Returns transactions by IDs, or an array with null values if not found ', async () => {
        const result = await client.query({
          query: makeTransactionQuery(`[
            "6ac19b8efd7114eea29080064b1ec6b5a10346a6212ee338d46f98b733851e3b",
            "3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843"
          ]`)
        })
        const notFoundResult = await client.query({
          query: makeTransactionQuery(`["?"]`)
        })
        const resultWithMissingTxs = await client.query({
          query: makeTransactionQuery(`[
            "?",
            "3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843"
          ]`)
        })
        expect(result).toMatchSnapshot()
        expect(notFoundResult).toMatchSnapshot()
        expect(resultWithMissingTxs).toMatchSnapshot()
      })
    })
  })
})

function makeTransactionQuery(ids: string) {
  return `{
              transactions(filter: { ids: ${ids}}) {
              id
              fee
              inputs {
                sourceTxId
                sourceTxIndex
                address
                value
              }
              outputs {
                value
                address
              }
            }
          }`
}
