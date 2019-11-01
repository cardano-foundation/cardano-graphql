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
    it('caps the response to 100 blocks', async () => {
      const result = await client.query({
        query: gql`query {
            blocks {
                id
            }
        }`
      })
      expect(result.data.blocks.length).toBe(100)
    })

    it('allows custom pagination size with a limit and offset', async () => {
      const page1 = await client.query({
        query: gql`query {
            blocks (limit: 20, offset: 3, order_by: { number: asc }) {
                id
                number
            }
        }`
      })
      const page2 = await client.query({
        query: gql`query {
            blocks (limit: 20, offset: 23, order_by: { number: asc }) {
                id
                number
            }
        }`
      })
      expect(page1.data.blocks.length).toBe(20)
      expect(page1.data.blocks[19].number).toBe(23)
      expect(page2.data.blocks.length).toBe(20)
      expect(page2.data.blocks[19].number).toBe(43)
    })

    it('Can return blocks by number', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
                where: { number: { _eq: 29022}}) {
                id
            }
        }`
      })
      expect(result.data.blocks.length).toBe(1)
      expect(result.data.blocks[0]).toEqual({ id: block29022.basic.id })
      expect(result).toMatchSnapshot()
    })

    it('Can return blocks by an array of IDs', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (
                where: { id: { _in: [
                    \"${block29021.basic.id}\",
                    \"${block29022.basic.id}\"
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
                transactions(order_by: { fee: desc}) {
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
      expect(result.data.blocks[0]).toEqual(block29021.basic)
      expect(result.data.blocks[1]).toEqual(block29022.basic)
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: gql`query {
            blocks( where: { number: { _eq: 29021 }}) {
                transactions_aggregate {
                    aggregate {
                        avg {
                            fee
                            totalOutput
                        }
                        count
                        max {
                            fee
                            totalOutput
                        }
                        min {
                            fee
                            totalOutput
                        }
                        sum {
                            fee
                            totalOutput
                        }
                    }
                }
                number
            }
        }`
      })
      expect(result.data.blocks[0]).toEqual(block29021.aggregated)
      expect(result).toMatchSnapshot()
    })

    it('are linked to their predecessor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (where: { number: { _eq: 29022}}) {
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
            epochs( where: { number: { _eq: 1 }}) {
                output
                number
                transactionsCount
                startedAt
                lastBlockTime
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1.basic)
      expect(result).toMatchSnapshot()
    })

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: 1 }}) {
                blocks_aggregate {
                    aggregate {
                        avg {
                            fees
                            size
                        }
                        count
                        max {
                            fees
                            size
                        }
                        min {
                            fees
                            size
                        }
                        sum {
                            fees
                            size
                        }
                    }
                }
                number
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1.aggregated)
      expect(result).toMatchSnapshot()
    })

    it('Returns epoch details by number range', async () => {
      const result = await client.query({
        query: gql`query {
            epochs( where: { number: { _in: [1] }}) {
                output
                number
                transactionsCount
                startedAt
                lastBlockTime
            }
        }`
      })
      expect(result.data.epochs[0]).toEqual(epoch1.basic)
      expect(result).toMatchSnapshot()
    })

    it('Can return aggregated Epoch data', async () => {
      const result = await client.query({
        query: gql`query {
            epochs_aggregate {
                aggregate {
                    count
                    max {
                        number
                        output
                        transactionsCount
                    }
                    min {
                        output
                        transactionsCount
                    }
                    sum {
                        output
                        transactionsCount
                    }
                }
            }
        }`
      })
      expect(result).toMatchSnapshot()
    })

    it('Returns blocks scoped to epoch', async () => {
      const validQueryResult = await client.query({
        query: gql`query {
            epochs( where: { number: { _eq: 1 }}) {
                blocks(limit: 1) {
                    epoch {
                        number
                    }
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
      expect(validQueryResult.data.epochs[0].blocks[0].epoch.number).toBe(1)
      expect(invalidQueryResult.data.epochs[0].blocks.length).toBe(0)
    })
  })

  describe('cardano', () => {
    it('Returns key information about the network', async () => {
      const result = await client.query({
        query: gql`query {
            cardano {
                blockHeight
                currentEpoch {
                    number
                }
                protocolConst
                slotDuration
                startTime
            }
        }`
      })
      expect(result).toMatchSnapshot()
    })
  })

  describe('transactions', () => {
    it('Returns transactions by IDs', async () => {
      const result = await client.query({
        query: gql`query {
            transactions(
                where: { id: { _in: [\"${txe68043.basic.id}\", \"${tx05ad8b.basic.id}\"]}},
                order_by: { fee: desc }
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

    it('Can return aggregated data', async () => {
      const result = await client.query({
        query: gql`query {
            transactions(
                where: { id: { _in: [\"${txe68043.aggregated.id}\", \"${tx05ad8b.aggregated.id}\"]}},
                order_by: { fee: desc }
            ) {
                fee
                id
                inputs_aggregate {
                    aggregate {
                        avg {
                            value
                        }
                        count
                        max {
                            value
                        }
                        min {
                            value
                        }
                        sum {
                            value
                        }
                    }
                }
                outputs_aggregate {
                    aggregate {
                        avg {
                            value
                        }
                        count
                        max {
                            value
                        }
                        min {
                            value
                        }
                        sum {
                            value
                        }
                    }
                }
            }
        }`
      })
      expect(result.data.transactions.length).toBe(2)
      const { transactions: txs } = result.data
      expect(txs).toEqual([tx05ad8b.aggregated, txe68043.aggregated])
      expect(txs[1].inputs_aggregate.aggregate.sum.value).toEqual(txs[1].outputs_aggregate.aggregate.sum.value + parseInt(txs[1].fee))
      expect(result).toMatchSnapshot()
    })
  })

  describe('utxoSet', () => {
    it('Can be scoped by address', async () => {
      const result = await client.query({
        query: gql`query {
            utxoSet(
                order_by: { address: asc }
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
                order_by: { address: asc }
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
