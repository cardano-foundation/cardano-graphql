import { expect } from 'chai'
import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import { Connection, ConnectionManager } from 'typeorm'
import { Context } from './Context'
import {
  BlockDataModel,
  Ledger,
  TxDataModel,
  TxInDataModel,
  TxOutDataModel
} from './data_sources'
import { resolvers } from './resolvers'

const connectionManager = new ConnectionManager()

describe('Integration', () => {
  let apolloServer: ApolloServerBase
  let client: ApolloServerTestClient
  let pgConnection: Connection

  beforeEach(async () => {
    pgConnection = connectionManager.create({
      database: 'cexplorer',
      host: 'localhost',
      password: 'postgres',
      port: 5432,
      username: 'nix',
      type: 'postgres',
      entities: [BlockDataModel, TxDataModel, TxInDataModel, TxOutDataModel]
    })
    await pgConnection.connect()
    apolloServer = new ApolloServerBase({
      dataSources (): Context['dataSources'] {
        return {
          ledger: new Ledger({
            transactions: pgConnection.getRepository(TxDataModel)
          })
        }
      },
      introspection: true,
      resolvers,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8')
    })
    client = createTestClient(apolloServer)
  })

  afterEach(async () => await pgConnection.close())

  describe('Ledger', () => {
    describe('transactions', () => {
      it('Returns transactions by IDs, or an array with null values if not found ', async () => {
        const result = await client.query({
          query: `query { transactions(ids: ["tx3", "tx2"]) { id fee } }`
        })
        // const notFoundResult = await client.query({
        //   query: `query { transactions(ids: ["tx?","tx??"]) { id fee } }`
        // })
        // const resultWithMissingTxs = await client.query({
        //   query: `query { transactions(ids: ["tx3", "tx?", "tx2"]) { id fee } }`
        // })
        expect(result.data.transactions).to.eql([{ id: 'tx3', fee: 5 }, { id: 'tx2', fee: 10 }])
        // expect(notFoundResult.data.transactions).to.deep.eq([null, null])
        // expect(resultWithMissingTxs.data.transactions).to.eql([{ id: 'tx3', fee: 5 }, null, { id: 'tx2', fee: 10 }])
      })
    })
  })
})
