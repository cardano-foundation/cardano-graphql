import { Transaction } from '../../graphql_types'
import { MempoolDataSource } from '../mempool'

export function InMemoryMempoolDataSource (data: { transactions: Transaction[]}): MempoolDataSource {
  return {
    async getTransaction (arg) {
      return data.transactions.find(({ id }) => arg === id) || null
    },
    async getTransactions (arg) {
      if (!arg) return data.transactions
      const results = data.transactions.filter(({ id }) => arg.includes(id))
      return results.length > 0 ? results : null
    },
    async has (id) {
      return !!data.transactions.find((tx) => tx.id === id)
    },
    async transactionCount () {
      return data.transactions.length
    }
  }
}
