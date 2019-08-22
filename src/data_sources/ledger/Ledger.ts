import { DataSource } from 'apollo-datasource'
import * as DataLoader from 'dataloader'
import { Block, Epoch, Transaction, TransactionOutput } from '../../graphql_types'

export class Ledger extends DataSource {

  block: DataLoader<Block['id'], Block>

  epoch: DataLoader<Epoch['number'], Epoch>

  transaction: DataLoader<Transaction['id'], Transaction>

  utxo: DataLoader<string, TransactionOutput>

  constructor () {
    super()
  }

  initialize (): void {
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(99)
  }
}
