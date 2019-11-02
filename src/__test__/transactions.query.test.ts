import gql from 'graphql-tag'
import { tx05ad8b, txe68043 } from './data_assertions'
import { TestClient } from './TestClient'

export function run (createClient: () => Promise<TestClient>) {
  describe('transactions', () => {
    let client: TestClient

    beforeEach(async () => {
      client = await createClient()
    }, 60000)

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
      expect(result.data).toMatchSnapshot()
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
      expect(result.data).toMatchSnapshot()
    })
  })
}
