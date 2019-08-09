import { DataSource } from 'apollo-datasource'
import { Transaction } from '../../graphql_types'

export interface LedgerProvider extends DataSource {
  blockHeight(): Promise<number>
  transaction(id: Transaction['id']): Promise<Transaction> | Promise<null>
  transactions(ids: Transaction['id'][]): Promise<Transaction[]> | Promise<null>
  has(id: Transaction['id']): Promise<boolean>
}
