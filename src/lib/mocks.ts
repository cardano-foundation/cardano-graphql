// import { Block, Epoch, Slot, Transaction } from '../graphql_types'
//
// export const outputs = [{
//   value: 100,
//   address: 'pk1'
// }, {
//   value: 200,
//   address: 'pk1'
// }]
//
// export const transactions = [{
//   fee: 10,
//   hash: 'someHash',
//   id: 'tx1',
//   inputs: [{
//     txId: 'tx1',
//     outputIndex: 0
//   }],
//   outputs
// }, {
//   fee: 20,
//   hash: 'someHash2',
//   id: 'tx2',
//   inputs: [{
//     txId: 'tx1',
//     outputIndex: 0
//   }],
//   outputs
// }] as Transaction[]
//
// export const blocks = [{
//   id: 'block1',
//   hash: 'someHash',
//   merkleRootHash: 'someHash',
//   number: 1,
//   size: 19,
//   transactions: transactions
// }] as Block[]
//
// export const slots = [{
//   block: blocks[0],
//   epoch
// }] as Slot[]
//
// export const epochs = [{
//   number: 1,
//   slots: slots
// }] as Epoch[]
