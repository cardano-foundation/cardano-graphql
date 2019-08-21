import { DataSource } from 'apollo-datasource'
import { Sequelize } from 'sequelize'
import * as DataLoader from 'dataloader'
import { Block, Transaction } from '../../graphql_types'
import { buildTxModel, buildTxInModel, buildTxOutModel } from './models'

export class Ledger extends DataSource {
  transaction: DataLoader<Transaction['id'], Transaction>

  block: DataLoader<Block['id'], Block>

  constructor (private sequelize: Sequelize) {
    super()
  }

  initialize (): void {

  }

  async blockById (id: Block['id']): Promise<Block> {
    const Tx = buildTxModel(this.sequelize)
    const TxIn = buildTxInModel(this.sequelize)
    const TxOut = buildTxOutModel(this.sequelize)
    return {
      id,
      transactions: await Tx.findAll({
        include: [{
          model: TxIn,
          include: [{
            model: TxOut,
            // This is undeniably unpleasant syntax, but this is the most
            // complicated join in the dataset
            on: {
              col1: Sequelize.where(Sequelize.col('txIns.tx_out_id'), '=', Sequelize.col('txIns->txOut.tx_id')),
              col2: Sequelize.where(Sequelize.col('txIns.tx_out_index'), '=', Sequelize.col('txIns->txOut.index'))
            }
          }]
        }, {
          model: TxOut
        }],
        // Query goes here
        where: {
          block: id
        }
      })
    }
  }

  blockHeight (): Promise<number> {
    return Promise.resolve(99)
  }
}
