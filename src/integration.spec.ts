import * as path from 'path'
import * as fs from 'fs'
import gql from 'graphql-tag'
import { createTestClient, ApolloServerTestClient } from 'apollo-server-testing'
import { ApolloServerBase } from 'apollo-server-core'
import * as depthLimit from 'graphql-depth-limit'
import {
  block29021, block29022,
  epoch1,
  tx05ad8b, txe68043
} from './lib/data_assertions'
import { getConfig } from './config'

describe('Integration', () => {
  let apolloServer: ApolloServerBase
  let client: ApolloServerTestClient

  beforeEach(async () => {
    process.env.HASURA_URI = 'http://localhost:8090/v1/graphql'
    const { context, resolvers } = await getConfig()
    apolloServer = new ApolloServerBase({
      context,
      introspection: true,
      resolvers,
      typeDefs: fs.readFileSync(path.join(__dirname, 'schema.graphql'), 'UTF8'),
      validationRules: [depthLimit(20)]
    })
    client = createTestClient(apolloServer)
  }, 60000)

  describe('blocks', () => {
    it('returns the most recent block by default ', async () => {
      const result = (await client.query({
        query: gql`query {
            blocks {
                number
            }
        }`
      })).data.blocks

      const blockHeight = (await client.query({
        query: gql`query {
            cardano {
                blockHeight
            }
        }`
      })).data.cardano.blockHeight

      expect(result[0].number).toEqual(blockHeight)
      expect(result).toMatchSnapshot()
    })
    it('throws an error if query requests more than 100 blocks', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (limit: 110) {
                id
            }
        }`
      })
      expect(result.errors[0]).toMatchSnapshot()
    })

    it('Uses pagination with an offset for larger result sets', async () => {
      const page1 = await client.query({
        query: gql`query {
            blocks (limit: 20, offset: 3, order_by: {number: asc}) {
                id
                number
            }
        }`
      })
      const page2 = await client.query({
        query: gql`query {
            blocks (limit: 20, offset: 23, order_by: {number: asc}) {
                id
                number
            }
        }`
      })
      console.log(page1)
      console.log('-----')
      console.log(page2)
      expect(page1.data.blocks).toMatchSnapshot()
      expect(page2.data.blocks).toMatchSnapshot()
    })

    it('Can return blocks by number', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
                limit: 2,
                where: { number: { _eq: ${block29022.number}}}) {
                id
            }
        }`
      })
      expect(result.data.blocks.length).toBe(1)
      expect(result.data.blocks[0]).toEqual({ id: block29022.id })
      expect(result).toMatchSnapshot()
    })

    it('Can return blocks by an array of IDs', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
                limit: 2,
                where: { id: { _in: [
                  \"${block29021.id}\",
                  \"${block29022.id}\"
                ]}},
                order_by: { number: asc }
            ) {
                epoch {
                    number
                }
                fees
                id
                merkelRootHash
                number
                previousBlock {
                    id
                    number
                }
                size
                slot {
                    number
                    startedAt
                }
                transactions(order_by: {fee: desc}) {
                    block {
                        number
                    }
                    fee
                    id
                    includedAt
                    inputs {
                        address
                        value
                    }

                    outputs {
                        value
                        address
                    }
                    totalOutput
                }
            }
        }`
      })
      expect(result.data.blocks.length).toBe(2)
      expect(result.data.blocks[0]).toEqual(block29021)
      expect(result.data.blocks[1]).toEqual(block29022)
      // expect(result.data.blocks[1]).toEqual(block43178)
      // expect(result).toMatchSnapshot()
    })
    it('are linked to their predecessor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (where: { number: { _eq: ${block29022.number}}}) {
                id
                previousBlock {
                    number
                    previousBlock {
                        number
                        previousBlock {
                            number
                        }
                    }
                }
            }
        }`
      })
      expect(result.data.blocks[0].previousBlock.previousBlock.previousBlock.number).toBe(29019)
      expect(result).toMatchSnapshot()
    })
  })

  describe('epochs', () => {
    it('Returns epoch details by number', async () => {
      const result = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: ${epoch1.number} }}) {
                endedAt
                output
                number
                startedAt
                transactionsCount
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1)
      expect(result).toMatchSnapshot()
    })
  })

  describe('cardano', () => {
    it('Returns the block height', async () => {
      const result = await client.query({
        query: gql`query {
            cardano {
                blockHeight
                currentEpoch {
                    number
                }
            }
        }`
      })
      expect(result.data.cardano.blockHeight).toBe(31070)
      expect(result.data.cardano.blockHeight).toMatchSnapshot()
    })
  })

  // describe('stakePools', () => {
  //   it('Returns Stake Pools', async () => {
  //     const result = await client.query({
  //       query: gql`query {
  //           stakePools {
  //               controlledStake
  //               id
  //               ticker
  //           }
  //       }`
  //     })
  //     expect(result).toMatchSnapshot()
  //   })
  // })

  describe('transactions', () => {
    it('Returns transactions by IDs', async () => {
      const result = await client.query({
        query: gql`query {
            transactions(
                limit: 2,
                where: { id: { _in: [\"${txe68043.id}\", \"${tx05ad8b.id}\"]}},
                order_by: {fee: desc}
            ) {
                block {
                    number
                }
                fee
                id
                inputs {
                    address
                    value
                }
                outputs {
                    address
                    value
                }
                totalOutput
            }
        }`
      })
      expect(result.data.transactions.length).toBe(2)
      expect(result).toMatchSnapshot()
    })

    describe('utxoSet', () => {
      it('Returns the whole set by default', async () => {
        const result = await client.query({
          query: gql`query {
              utxoSet(
                  limit: 20
              ) {
                  address
                  value
              }
          }`
        })
        expect(result.data.utxoSet.length).toBe(20)
        expect(result).toMatchSnapshot()
      })
      it('Can be filtered by address', async () => {
        const result = await client.query({
          query: gql`query {
              utxoSet(
                  where: { address: { _eq:
                  "DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s"
                  }
                  }
              ) {
                  address
                  value
              }
          }`
        })
        expect(result.data.utxoSet.length).toBe(1)
        expect(result).toMatchSnapshot()
      })
    })
  })
})
