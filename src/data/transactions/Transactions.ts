import { DataSource } from 'apollo-datasource'
import { Transaction } from '../../graphql_types'

export interface Transactions extends DataSource {
  findById(id: Transaction['id']): Promise<Transaction> | Promise<null>,
  findByIds(ids: Transaction['id'][]): Promise<Transaction[]> | Promise<null>
  has(id: Transaction['id']): Promise<boolean>
}
