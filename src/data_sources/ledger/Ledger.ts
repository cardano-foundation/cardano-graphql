import { DataSource } from 'apollo-datasource'
import * as DataLoader from 'dataloader'
import { Block, Transaction } from '../../graphql_types'
import { EntityLoader } from '../../lib/data_loaders/'

export class Ledger extends DataSource {
  transaction: DataLoader<Transaction['id'], Transaction>

  block: DataLoader<Block['id'], Block>

  constructor (private data: {
    blocks: Block[],
    transactions: Transaction[]
  }) {
    super()
  }

  initialize (): void {
    this.block = EntityLoader<Block>(this.data.blocks)
    this.transaction = EntityLoader<Transaction>(this.data.transactions)
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(this.data.blocks.length)
  }
}
