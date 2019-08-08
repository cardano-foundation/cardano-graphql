import { Transaction } from '../../graphql_types'

export const testTransactions = [{
  id: 'tx2',
  fee: 10,
  inputs: [{
    outpoint: {
      txId: 'tx1',
      outputIndex: 0
    }
  }],
  outputs: [{
    value: 100,
    address: 'pk1'
  }]
}, {
  id: 'tx3',
  fee: 5,
  inputs: [{
    outpoint: {
      txId: 'tx2',
      outputIndex: 0
    }
  }],
  outputs: [{
    value: 50,
    address: 'pk2'
  }]
}] as Transaction[]
