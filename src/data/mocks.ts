import { Block, Transaction } from '../graphql_types'

// Todo: Use faker to build this dynamically

export const transactions = [{
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

export const blocks = [{
  id: 'block1',
  transactions
}] as Block[]
