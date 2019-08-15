import { DataSource } from 'apollo-datasource'
import * as DataLoader from 'dataloader'
import { Transaction } from '../../graphql_types'
import { EntityLoader } from '../EntityLoader'

export class Mempool extends DataSource {
  transaction: DataLoader<Transaction['id'], Transaction>

  constructor (private data: {
    transactions: Transaction[]
  }) {
    super()
  }

  initialize (): void {
    this.transaction = EntityLoader<Transaction>(this.data.transactions)
  }

  transactionCount (): Promise<number> {
    return Promise.resolve(this.data.transactions.length)
  }
}
