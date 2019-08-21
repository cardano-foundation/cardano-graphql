import { BLOB, INTEGER, Model, Sequelize } from 'sequelize'

class TxOut extends Model { }

export function buildTxOutModel (sequelize: Sequelize) {
  TxOut.init({
    id: {
      type: INTEGER,
      primaryKey: true
    },
    tx_id: {
      type: INTEGER
    },
    index: {
      type: INTEGER
    },
    address: {
      type: BLOB
    },
    value: {
      type: INTEGER
    }
  }, {
    sequelize,
    modelName: 'txOut',
    tableName: 'tx_out',
    timestamps: false
  })
  return TxOut
}
