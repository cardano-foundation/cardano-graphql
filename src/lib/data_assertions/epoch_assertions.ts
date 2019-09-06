import { Currency, Epoch } from '../../graphql_types'
import { block43177, block43178 } from './block_assertions'
import { stakePool1, stakePool2 } from './stakePool_assertions'

export const epoch2 = {
  blocks: [block43177, block43178],
  endedAt: new Date(Date.UTC(2017, 10, 9, 7, 44, 51)),
  // output: [{ currency: Currency.Ada, value: 21797815347185914 }], // Correct, but larger than BigInt
  output: [{
    currency: Currency.Ada,
    amount: 217978153471859 // Missing the last digit to fit into BigInt bounds
  }],
  number: 2,
  slots: [{
    block: {
      number: block43177.number
    },
    epoch: {
      number: 2
    },
    number: 1
  }, {
    block: {
      number: block43178.number
    },
    epoch: {
      number: 2
    },
    number: 2
  }],
  stakeDistribution: [stakePool1, stakePool2],
  startedAt: new Date(Date.UTC(2017, 10, 4, 7, 44, 51)),
  transactions: [
    block43177.transactions[0],
    block43177.transactions[1],
    block43178.transactions[0]
  ],
  transactionsCount: 4292
} as Epoch
