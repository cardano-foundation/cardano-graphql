const Sequelize = require('sequelize')
const Model = Sequelize.Model
const Op = Sequelize.Op

// Option 1: Passing parameters separately
const sequelize = new Sequelize('cexplorer', 'nix', 'postgres', {
  host: 'localhost',
  dialect: 'postgres'
})

class Tx extends Model { }
Tx.init({
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true
  },
  hash: {
    type: Sequelize.BLOB
  },
  block: {
    type: Sequelize.INTEGER,
  },
  fee: {
    type: Sequelize.INTEGER,
  }
}, {
    sequelize,
    modelName: 'tx',
    tableName: 'tx',
    timestamps: false
  })

class TxIn extends Model { }
TxIn.init({
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true
  },
  tx_in_id: {
    type: Sequelize.INTEGER
  },
  tx_out_id: {
    type: Sequelize.INTEGER
  },
  tx_out_index: {
    type: Sequelize.INTEGER,
  }
}, {
    sequelize,
    modelName: 'txIn',
    tableName: 'tx_in',
    timestamps: false
  })

class TxOut extends Model { }
TxOut.init({
  id: {
    type: Sequelize.INTEGER,
    primaryKey: true
  },
  tx_id: {
    type: Sequelize.INTEGER
  },
  index: {
    type: Sequelize.INTEGER,
  },
  address: {
    type: Sequelize.BLOB,
  },
  value: {
    type: Sequelize.INTEGER,
  }
}, {
    sequelize,
    modelName: 'txOut',
    tableName: 'tx_out',
    timestamps: false
  })

Tx.hasMany(TxIn, { foreignKey: 'tx_in_id' })
Tx.hasMany(TxOut, { foreignKey: 'tx_id' })
TxIn.belongsTo(TxOut, { foreignKey: 'tx_out_id', targetKey: 'tx_id' })

async function example() {
  const beforeQuery = Date.now()

  const txs = await Tx.findAll({
    include: [{
      model: TxIn,
      include: [{
        model: TxOut,
        // This is undeniably unpleasant syntax, but this is the most
        // complicated join in the dataset
        on: {
          col1: sequelize.where(sequelize.col("txIns.tx_out_id"), "=", sequelize.col("txIns->txOut.tx_id")),
          col2: sequelize.where(sequelize.col("txIns.tx_out_index"), "=", sequelize.col("txIns->txOut.index"))
        }
      }]
    }, {
      model: TxOut
    }],
    // Query goes here
    where: {
      block: 50959
    }
  })

  const afterQuery = Date.now()
  const numberOfTxs = txs.length
  const mappedTxs = txs.map(prettyPrintTx)
  const afterMap = Date.now()

  console.log(`Sample Tx: `, JSON.stringify(mappedTxs[numberOfTxs - 1], null, 2))
  console.log(`Transaction count: ${numberOfTxs}`)
  console.log(`Query time (s): ${(afterQuery - beforeQuery) / 1000}`)
  console.log(`Mapping time (s): ${(afterMap - afterQuery) / 1000}`)
}

function prettyPrintTx(tx) {
  const hash = tx.hash.toString('hex')
  return {
    hash,
    fee: tx.fee,
    block: tx.block,
    inputs: tx.txIns.map(input => {
      return {
        address: input.txOut.address.toString('hex'),
        value: input.txOut.value
      }
    }),
    outputs: tx.txOuts.map(out => {
      return {
        index: out.index,
        address: out.address.toString('hex'),
        value: out.value
      }
    })
  }
}

example()