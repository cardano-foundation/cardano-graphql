import * as path from 'path'
import * as fs from 'fs'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import * as depthLimit from 'graphql-depth-limit'
import { resolvers } from './resolvers'
import {
  block43177, block43178,
  epoch2,
  tx21c528, txa54489, txd9e280
} from './lib/data_assertions'
import * as queries from './lib/queries'
import { buildHasuraSchema } from './lib/buildHasuraSchema'
import { getConfig } from './config'

describe('Integration', () => {
  let apolloServer: ApolloServerBase
  let client: ApolloServerTestClient

  beforeEach(async () => {
    const config = getConfig()
    const hasura = await buildHasuraSchema(config.hasuraUri)
    apolloServer = new ApolloServerBase({
      context: () => ({ hasura }),
      introspection: true,
      resolvers,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8'),
      validationRules: [depthLimit(20)]
    })
    client = createTestClient(apolloServer)
  })

  describe('blocks', () => {
    // it('returns a single result by default', async () => {
    // const result = await client.query({
    //   query: queries.blocksWithNoTx
    // })
    // expect(result.data.blocks.length).toBe(1)
    // })

    it('throws an error if query requests more than 100 blocks', async () => {
      const result = await client.query({
        query: queries.blocksWithNoTx,
        variables: {
          limit: 110
        }
      })
      expect(result.errors[0]).toMatchSnapshot()
    })

    it('Uses pagination with an offset for larger result sets', async () => {
      const page1 = await client.query({
        query: queries.blocksWithNoTx,
        variables: {
          limit: 100
        }
      })
      const page2 = await client.query({
        query: queries.blocksWithNoTx,
        variables: {
          limit: 100,
          offset: 100
        }
      })
      expect(page1.data.blocks).toMatchSnapshot()
      expect(page2.data.blocks).toMatchSnapshot()
    })

    it('can be accessed by number, with the return order in he array guaranteed', async () => {
      const result = await client.query({
        query: queries.blocksWithNoTx,
        variables: {
          limit: 2,
          where: {
            number: {
              _in: [block43178.number, block43177.number]
            }
          }
        }
      })
      // expect(result.data.blocks.length).toBe(2)
      // expect(result.data.blocks[0]).toEqual(block43178)
      // expect(result.data.blocks[1]).toEqual(block43177)
      expect(result).toMatchSnapshot()
    })

    it('Returns blocks by id', async () => {
      const result = await client.query({
        query: queries.blocksWithNoTx,
        variables: {
          limit: 2,
          where: {
            id: {
              _in: [block43178.id, block43177.id]
            }
          }
        }
      })
      expect(result.data.blocks.length).toBe(2)
      // expect(result.data.blocks[0]).toEqual(block43177)
      // expect(result.data.blocks[1]).toEqual(block43178)
      expect(result).toMatchSnapshot()
    })
    it('are linked to their predecessor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: queries.nestedBlocks,
        variables: {
          where: {
            number: {
              _eq: block43178.number
            }
          }
        }
      })
      // expect(result.data.blocks[0].previousBlock).toEqual(block43177)
      // expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toBe(7)
      expect(result).toMatchSnapshot()
    })
  })
  //
  describe('epochs', () => {
    it('Returns epoch details by number', async () => {
      const result = await client.query({
        query: queries.epochDetails,
        variables: {
          where: {
            number: { _eq: epoch2.number }
          }
        }
      })
      // const resultWithConstructedDates = {
      //   ...result.data.epochs[0],
      //   endedAt: new Date(result.data.epochs[0].endedAt),
      //   startedAt: new Date(result.data.epochs[0].startedAt)
      // }
      // expect(resultWithConstructedDates).toEqual(epoch2)
      expect(result).toMatchSnapshot()
    })
  })

  describe('ledger', () => {
    it('Returns the block height', async () => {
      const result = await client.query({
        query: queries.ledger
      })
      expect(result.data.ledger.blockHeight).toBe(99)
      expect(result.data.ledger.blockHeight).toMatchSnapshot()
    })
  })

  describe('transactions', () => {
    it('Returns transactions by IDs', async () => {
      const result = await client.query({
        query: queries.transactions,
        variables: {
          limit: 2,
          where: {
            id: {
              _in: [tx21c528.id, txd9e280.id]
            }
          }
        }
      })
      // expect(result.data.transactions.length).toBe(2)
      // expect(result.data.transactions).toEqual([
      //   block43177.transactions[0],
      //   block43178.transactions[0]
      // ])
      expect(result).toMatchSnapshot()
    })

    it('Returns null if for specific transaction lookup', async () => {
      const result = await client.query({
        query: queries.transactions,
        variables: {
          limit: 2,
          where: {
            id: {
              _in: ['?', txa54489.id, '??']
            }
          }
        }
      })
      // expect(result.data.transactions.length).toBe(3)
      // expect(result.data.transactions[0]).toBe(null)
      // expect(result.data.transactions[1]).toEqual(txa54489)
      // expect(result.data.transactions[2]).toBe(null)
      expect(result).toMatchSnapshot()
    })

    describe('utxoSet', () => {
      it('Returns the whole set by default', async () => {
        const result = await client.query({
          query: queries.utxoSet
        })
        expect(result).toMatchSnapshot()
      })
      it('Can be filtered by address', async () => {
        const result = await client.query({
          query: queries.utxoSet,
          variables: {
            where: {
              address: { _eq: 'DdzFFzCqrhsr1WxmzVcLWpTwqQQJkk9Be4SpM3VdWaT892biB9rCVFKgbsRPAHu484jPhjE5e57S9cCzF8yKwXhLgri2SnvSMZtLB87y' }
            }
          }
        })
        expect(result).toMatchSnapshot()
      })
    })
  })
})
