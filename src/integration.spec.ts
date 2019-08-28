import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import { Context } from './Context'
import { BlockRepository, Ledger, TransactionRepository } from './data_sources'
import { getConfig } from './config'
import { resolvers } from './resolvers'
import { block43177, block43178 } from './lib/block_assertions'
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
                block43177.transactions[0].id,
                block43178.transactions[0].id
              ]
            }
          }
        })
        expect(result.data.transactions.length).toBe(2)
        expect(result.data.transactions[0].fee).toBe(block43177.transactions[0].fee)
        expect(result.data.transactions[1].fee).toBe(block43178.transactions[0].fee)
        // expect(result.data.transactions[0]).toEqual({
        //   blockNo: block43177.number,
        //   ...block43177.transactions[0]
        // })
        // expect(result.data.transactions[0]).toEqual({
        //   blockNo: block43178.number,
        //   ...block43178.transactions[0]
        // })
        expect(result).toMatchSnapshot()
      })

      it('Returns null if for specific transaction lookup', async () => {
        const resultWithMissingTxs = await client.query({
          query: queries.transactions,
          variables: {
            filter: {
              ids: [
                '?',
                block43177.transactions[0].id,
                '??'
              ]
            }
          }
        })
        expect(resultWithMissingTxs.data.transactions.length).toBe(3)
        expect(resultWithMissingTxs.data.transactions[0]).toEqual(null)
        // expect(resultWithMissingTxs.data.transactions[1]).toEqual({
        //   blockNo: block43177.number,
        //   ...block43177.transactions[0]
        // })
        expect(resultWithMissingTxs.data.transactions[1].fee).toBe(block43177.transactions[0].fee)
        expect(resultWithMissingTxs.data.transactions[2]).toEqual(null)
        expect(resultWithMissingTxs).toMatchSnapshot()
      })

      it('Can return transactions in blocks as a nested array', async () => {
        const transactionsByBlock = await client.query({
          query: queries.blocksWithTxs,
          variables: {
            filter: {
              numbers: [block43177.number]
            }
          }
        })
        expect(transactionsByBlock.data.blocks[0].transactions.length).toBe(block43177.transactions.length)
        expect(transactionsByBlock).toMatchSnapshot()
      })
    })

    describe('blocks', () => {
      it('Returns blocks by number', async () => {
        const result = await client.query({
          query: queries.blocksWithNoTx,
          variables: {
            filter: {
              numbers: [1, block43177.number, 30]
            }
          }
        })
        expect(result.data.blocks.length).toBe(3)
        // expect(block43177).toContainEqual(result.data.blocks[1])
        expect(result).toMatchSnapshot()
      })
      it('Returns blocks by id', async () => {
        const result = await client.query({
          query: queries.blocksWithNoTx,
          variables: {
            filter: {
              ids: [
                '1dbc81e3196ba4ab9dcb07e1c37bb28ae1c289c0707061f28b567c2f48698d50',
                block43177.id
              ]
            }
          }
        })
        expect(result.data.blocks.length).toBe(2)
        // expect(block43177).toContainEqual(result.data.blocks[1])
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
