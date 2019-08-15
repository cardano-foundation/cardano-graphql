import { expect } from 'chai'
import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import { Mempool, Ledger } from './data_sources'
import { blocks, transactions } from './lib/mocks'
import { resolvers } from './resolvers'
import { Context } from './Context'

describe('Integration', () => {
  let server: ApolloServerBase
  let client: ApolloServerTestClient

  beforeEach(() => {
    server = new ApolloServerBase({
      dataSources (): Context['dataSources'] {
        return {
          ledger: new Ledger({ blocks, transactions }),
          mempool: new Mempool({ transactions })
        }
      },
      introspection: true,
      resolvers,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
    })
    client = createTestClient(server)
  })

  describe('Ledger', () => {
    describe('Transactions', () => {
      it('Returns a transaction by ID, or null data value if not found', async () => {
        const result = await client.query({
          query: `query { transaction(id: "tx3") { id fee }}`
        })
        const notFoundResult = await client.query({
          query: `query { transaction(id: "tx?") { id fee }}`
        })
        expect(result.data.transaction).to.eql({ id: 'tx3', fee: 5 })
        expect(notFoundResult.data).to.eql(null)
      })
      it('Returns transactions by IDs, returning null data value for no results, or null values in the array if a partial set is found ', async () => {
        const result = await client.query({
          query: `query { transactions(ids: ["tx3", "tx2"]) { id fee } }`
        })
        const notFoundResult = await client.query({
          query: `query { transaction(id: "tx?") { id fee } }`
        })
        const resultWithMissingTxs = await client.query({
          query: `query { transactions(ids: ["tx3", "tx?", "tx2"]) { id fee } }`
        })
        expect(result.data.transactions).to.eql([{ id: 'tx3', fee: 5 }, { id: 'tx2', fee: 10 }])
        expect(notFoundResult.data).to.eql(null)
        expect(resultWithMissingTxs.data.transactions).to.eql([{ id: 'tx3', fee: 5 }, null, { id: 'tx2', fee: 10 }])
      })
    })
  })
})
