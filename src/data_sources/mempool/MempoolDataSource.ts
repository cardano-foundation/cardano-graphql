import { DataSource } from 'apollo-datasource'
import { Transaction } from '../../graphql_types'

export interface MempoolDataSource extends DataSource {
  getTransaction(id: Transaction['id']): Promise<Transaction> | Promise<null>,
  getTransactions(ids?: Transaction['id'][]): Promise<Transaction[]> | Promise<null>
  has(id: Transaction['id']): Promise<boolean>
  transactionCount(): Promise<number>
}
