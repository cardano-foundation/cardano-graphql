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
    it('throws an error if query requests more than 100 blocks', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (where: { number: { _neq: 1 }}, limit: 110) {
                id
            }
        }`
      })
      expect(result.errors[0]).toMatchSnapshot()
    })

    it('Uses pagination with an offset for larger result sets', async () => {
      const page1 = await client.query({
        query: gql`query {
            blocks (where: { epoch: { number: { _eq: 1 }}}, limit: 20, offset: 3, order_by: {number: asc}) {
                id
                number
            }
        }`
      })
      const page2 = await client.query({
        query: gql`query {
            blocks (where: { epoch: { number: { _eq: 1 }}}, limit: 20, offset: 23, order_by: {number: asc}) {
                id
                number
            }
        }`
      })
      expect(page1.data.blocks.length).toBe(20)
      expect(page1.data.blocks[19].number).toBe(21609)
      expect(page2.data.blocks.length).toBe(20)
      expect(page2.data.blocks[19].number).toBe(21629)
    })

    it('Can return blocks by number', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
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
                createdAt
                previousBlock {
                    id
                    number
                }
                size
                slotNo
                slotWithinEpoch
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
    })
    it('limits the result set to 100 if only an exclusion is provided', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
                where: { number: { _neq: ${block29022.number}}}) {
                id
            }
        }`
      })
      expect(result.data.blocks.length).toBe(100)
    })
    it('caps the set specified in the where clause if is too large', async () => {
      const result = await client.query({
        query: gql`query {
            blocks(where: {number: {_in: [
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
                51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
                76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
                101
            ]}}) {
                id
            }
        }`
      })
      expect(result.data.blocks.length).toEqual(100)
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
                output
                number
                transactionsCount
                startedAt
                lastBlockTime
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1)
      expect(result).toMatchSnapshot()
    })

    it('Returns epoch details by number range', async () => {
      const result = await client.query({
        query: gql`query {
            epochs( where: { number: { _in: [${epoch1.number}] }}) {
                output
                number
                transactionsCount
                startedAt
                lastBlockTime
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1)
      expect(result).toMatchSnapshot()
    })

    it('Returns blocks scoped to epoch', async () => {
      const validQueryResult = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: 1 }}) {
                blocks(limit: 20) {
                    id
                }
            }
        }`
      })
      const invalidQueryResult = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: 1 }}) {
                blocks(limit: 20, where: { epoch: { number: { _eq: 0 } }}) {
                    id
                }
            }
        }`
      })
      expect(validQueryResult.data.epochs[0].blocks.length).toBe(20)
      expect(invalidQueryResult.data.epochs[0].blocks.length).toBe(0)
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

  describe('transactions', () => {
    it('Returns transactions by IDs', async () => {
      const result = await client.query({
        query: gql`query {
            transactions(
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
      it('Can be scoped by address', async () => {
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
        expect(result.data.utxoSet.length).toBe(2)
        expect(result).toMatchSnapshot()
      })
      it('Can be scoped by list of addresses', async () => {
        const result = await client.query({
          query: gql`query {
              utxoSet(
                  where: { address: { _in: [
                      "DdzFFzCqrhskotfhVwhLvNFaVGpA6C4yR9DXe56oEL4Ewmze51f1uQsc1cQb8qUyqgzjUPBgFZiVbuQu7BaXrQkouyvzjYjLqfJpKG5s",
                      "Ae2tdPwUPEZGvXJ3ebp4LDgBhbxekAH2oKZgfahKq896fehv8oCJxmGJgLt"
                  ]}}
              ) {
                  address
                  value
              }
          }`
        })
        expect(result.data.utxoSet.length).toBe(3)
        expect(result).toMatchSnapshot()
      })
    })
  })
})
