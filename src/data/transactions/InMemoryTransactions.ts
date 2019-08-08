import { Transaction } from '../../graphql_types'
import { Transactions } from './'

export function InMemoryTransactions (data: Transaction[]): Transactions {
  return {
    async findById (arg) {
      return data.find(({ id }) => arg === id) || null
    },
    async findByIds (arg) {
      const results = data.filter(({ id }) => arg.includes(id))
      return results.length > 0 ? results : null
    },
    async has (id) {
      return !!data.find((tx) => tx.id === id)
    }
  }
}
