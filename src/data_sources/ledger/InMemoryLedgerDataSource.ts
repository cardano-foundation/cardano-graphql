import { Transaction } from '../../graphql_types'
import { LedgerDataSource } from '.'

export function InMemoryLedgerDataSource (data: { transactions: Transaction[]}): LedgerDataSource {
  return {
    async blockHeight () {
      return data.transactions.length
    },
    async transaction (arg) {
      return data.transactions.find(({ id }) => arg === id) || null
    },
    async transactions (arg) {
      const results = data.transactions.filter(({ id }) => arg.includes(id))
      return results.length > 0 ? results : null
    },
    async has (id) {
      return !!data.transactions.find((tx) => tx.id === id)
    }
  }
}
