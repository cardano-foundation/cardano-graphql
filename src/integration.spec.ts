import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import { Context } from './Context'
import { BlockRepository, Ledger, TransactionRepository } from './data_sources'
import { getConfig } from './config'
import { resolvers } from './resolvers'
import * as queries from './lib/queries'

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
            blocks: postgres.getCustomRepository(BlockRepository),
            transactions: postgres.getCustomRepository(TransactionRepository)
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
    describe('Ledger information query', () => {
      it('Return the block height', async () => {
        const result = await client.query({
          query: queries.ledgerMeta
        })
        expect(result.data.ledger.blockHeight).toBe(1298333403)
        expect(result.data.ledger.blockHeight).toMatchSnapshot()
      })
    })

    describe('transactions', () => {
      it('Returns transactions by IDs', async () => {
        const result = await client.query({
          query: queries.transactions,
          variables: {
            filter: {
              ids: [
                '6ac19b8efd7114eea29080064b1ec6b5a10346a6212ee338d46f98b733851e3b',
                '3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843'
              ]
            }
          }
        })
        expect(result.data.transactions.length).toBe(2)
        expect(result.data.transactions[0].id).toEqual('6ac19b8efd7114eea29080064b1ec6b5a10346a6212ee338d46f98b733851e3b')
        expect(result.data.transactions[1].id).toEqual('3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843')
        expect(result).toMatchSnapshot()
      })

      it('Returns null if for specific transaction lookup', async () => {
        const resultWithMissingTxs = await client.query({
          query: queries.transactions,
          variables: {
            filter: {
              ids: [
                '?',
                '3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843',
                '??'
              ]
            }
          }
        })
        expect(resultWithMissingTxs.data.transactions.length).toBe(3)
        expect(resultWithMissingTxs.data.transactions[0]).toEqual(null)
        expect(resultWithMissingTxs.data.transactions[1].id).toEqual('3ec59e9b74e297f4a60ea026baa225ce4ae8fde2b017ad1eb2b691acc1d0a843')
        expect(resultWithMissingTxs.data.transactions[2]).toEqual(null)
        expect(resultWithMissingTxs).toMatchSnapshot()
      })

      it('Can return transactions in blocks as a nested array', async () => {
        const transactionsByBlock = await client.query({
          query: queries.blocksWithSomeTx,
          variables: {
            filter: {
              numbers: [1]
            },
            txLimit: 33
          }
        })
        expect(transactionsByBlock.data.blocks[0].transactions.length).toBe(33)
        expect(transactionsByBlock).toMatchSnapshot()
      })
    })

    describe('blocks', () => {
      it('Returns blocks by number', async () => {
        const result = await client.query({
          query: queries.blocksWithNoTx,
          variables: {
            filter: {
              numbers: [1, 2, 30]
            }
          }
        })
        expect(result.data.blocks.length).toBe(3)
        expect(result.data.blocks[2].number).toEqual(30)
        expect(result).toMatchSnapshot()
      })
      it('Returns blocks by id', async () => {
        const result = await client.query({
          query: queries.blocksWithNoTx,
          variables: {
            filter: {
              ids: [
                '1dbc81e3196ba4ab9dcb07e1c37bb28ae1c289c0707061f28b567c2f48698d50',
                'd3fdc8c8ea4050cc87a21cb73110d54e3ec92f8ae76941e8a1957ed6e6a7e0b0'
              ]
            }
          }
        })
        expect(result.data.blocks.length).toBe(2)
        expect(result.data.blocks[1].number).toEqual(30)
        expect(result).toMatchSnapshot()
      })
      // it('Can return previous blocks', async () => {
      //   const result = await client.query({
      //     query: queries.nestedBlocks,
      //     variables: {
      //       filter: {
      //         numbers: [10]
      //       }
      //     }
      //   })
      //   expect(result.data.blocks.length).toBe(1)
      //   expect(result.data.blocks[0].previousBlock.number).toEqual(9)
      //   expect(result.data.blocks[0].previousBlock.previousBlock.number).toEqual(8)
      //   expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toEqual(7)
      //   expect(result).toMatchSnapshot()
      // })
    })
  })
})
