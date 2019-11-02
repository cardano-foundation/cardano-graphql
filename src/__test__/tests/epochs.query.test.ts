import gql from 'graphql-tag'
import { epoch1 } from '../data_assertions'
import { TestClient } from '../TestClient'

export function epochTests (createClient: () => Promise<TestClient>) {
  describe('epochs', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

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
      expect(result.data).toMatchSnapshot()
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
      expect(result.data).toMatchSnapshot()
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
      expect(result.data).toMatchSnapshot()
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
      expect(result.data).toMatchSnapshot()
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
}
