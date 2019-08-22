import { Connection } from 'typeorm'
import { DataSource } from 'apollo-datasource'
import * as DataLoader from 'dataloader'
import alignDataLoaderValues from 'dataloader-values'
import { Block, Epoch, Transaction, TransactionOutput } from '../../graphql_types'
import * as mocks from '../../lib/mocks'

export class Ledger extends DataSource {
  private connection: Connection
  block: DataLoader<Block['id'], Block>
  epoch: DataLoader<Epoch['number'], Epoch>
  transaction: DataLoader<Transaction['id'], Transaction>
  utxo: DataLoader<string, TransactionOutput>

  constructor (connection: Connection) {
    super()
    this.connection = connection
  }

  initialize (): void {
    this.block = new DataLoader<Block['id'], Block>((ids: Block['id'][]) => {
      return Promise.resolve(alignDataLoaderValues({
        keys: ids,
        values: mocks.blocks,
        getKey: ({ id }) => id
      }))
    })
    this.epoch = new DataLoader<Epoch['number'], Epoch>((numbers: Epoch['number'][]) => {
      return Promise.resolve(mocks.epochs)
    })
    this.transaction = new DataLoader<Transaction['id'], Transaction>((ids: Transaction['id'][]) => {
      return Promise.resolve(mocks.transactions)
    })
    this.utxo = new DataLoader<string, TransactionOutput>((addresses: string[]) => {
      return Promise.resolve(mocks.outputs)
    })
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(99)
  }
}
