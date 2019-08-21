import { BLOB, INTEGER, Model, Sequelize } from 'sequelize'
import { buildTxInModel, buildTxOutModel } from '.'

class TxModel extends Model {
  public id!: string
}
export function buildTxModel (sequelize: Sequelize) {
  TxModel.init({
    id: {
      type: INTEGER,
      primaryKey: true
    },
    hash: {
      type: BLOB
    },
    block: {
      type: INTEGER
    },
    fee: {
      type: INTEGER
    }
  }, {
    sequelize,
    modelName: 'tx',
    tableName: 'tx',
    timestamps: false
  })
  const TxIn = buildTxInModel(sequelize)
  const TxOut = buildTxOutModel(sequelize)
  TxModel.hasMany(TxIn, { foreignKey: 'tx_in_id' })
  TxModel.hasMany(TxOut, { foreignKey: 'tx_id' })
  return TxModel
}
