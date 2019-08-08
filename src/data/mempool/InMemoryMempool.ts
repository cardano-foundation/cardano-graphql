import { Transaction } from '../../graphql_types'
import { MempoolProvider } from './'

export function InMemoryMempool (data: Transaction[]): MempoolProvider {
  return {
    async transactions () {
      return data
    },
    async transactionById (arg) {
      return data.find(({ id }) => arg === id) || null
    },
    async transactionsByIds (arg) {
      const results = data.filter(({ id }) => arg.includes(id))
      return results.length > 0 ? results : null
    },
    async has (id) {
      return !!data.find((tx) => tx.id === id)
    },
    async transactionCount () {
      return data.length
    }
  }
}
