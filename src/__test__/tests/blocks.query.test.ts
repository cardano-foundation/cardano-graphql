import gql from 'graphql-tag'
import { block29021, block29022 } from '../data_assertions'
import { TestClient } from '../TestClient'

export function blocksTests (makeClient: () => Promise<TestClient>) {
  describe('blocks', () => {
    let client: TestClient
    beforeEach(async () => {
      client = await makeClient()
    }, 60000)

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
      expect(result.data).toMatchSnapshot()
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
                epochNo
                fees
                id
                merkelRootHash
                number
                createdAt
                createdBy
                previousBlock {
                    id
                    number
                }
                nextBlock {
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
      expect(result.data).toMatchSnapshot()
    })

    it('Can return filtered aggregated data', async () => {
      const result = await client.query({
        query: gql`query {
            blocks( where: { number: { _eq: 29021 }}) {
                transactions_aggregate( 
                    where: {
                        _and: [
                            { fee: { _gt: 10 }},
                            { totalOutput: { _lt: "4924799478670" } }
                        ]
                    }) {
                    aggregate {
                        count
                    }
                }
                number
            }
        }`
      })
      expect(result.data.blocks[0]).toEqual(block29021.aggregated_filtered)
      expect(result.data).toMatchSnapshot()
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
      expect(result.data).toMatchSnapshot()
    })

    it('are linked to their successor, and the chain can be traversed', async () => {
      const result = await client.query({
        query: gql`query {
            blocks (where: { number: { _eq: 29022}}) {
                id
                nextBlock {
                    number
                    nextBlock {
                        number
                        nextBlock {
                            number
                        }
                    }
                }
            }
        }`
      })
      expect(result.data.blocks[0].nextBlock.nextBlock.nextBlock.number).toBe(29025)
      expect(result.data).toMatchSnapshot()
    })
  })
}
