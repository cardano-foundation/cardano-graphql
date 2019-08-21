import { INTEGER, Model, Sequelize } from 'sequelize'
import { buildTxOutModel } from '.'

class TxIn extends Model { }

export function buildTxInModel (sequelize: Sequelize) {
  TxIn.init({
    id: {
      type: INTEGER,
      primaryKey: true
    },
    tx_in_id: {
      type: INTEGER
    },
    tx_out_id: {
      type: INTEGER
    },
    tx_out_index: {
      type: INTEGER
    }
  }, {
    sequelize,
    modelName: 'txIn',
    tableName: 'tx_in',
    timestamps: false
  })
  const TxOut = buildTxOutModel(sequelize)
  TxIn.belongsTo(TxOut, { foreignKey: 'tx_out_id', targetKey: 'tx_id' })
  return TxIn
}
