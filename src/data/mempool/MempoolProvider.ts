import { DataSource } from 'apollo-datasource'
import { Transaction } from '../../graphql_types'

export interface MempoolProvider extends DataSource {
  transactions(): Promise<Transaction[]> | Promise<null>
  transactionById(id: Transaction['id']): Promise<Transaction> | Promise<null>,
  transactionsByIds(ids: Transaction['id'][]): Promise<Transaction[]> | Promise<null>
  has(id: Transaction['id']): Promise<boolean>
  transactionCount(): Promise<number>
}
