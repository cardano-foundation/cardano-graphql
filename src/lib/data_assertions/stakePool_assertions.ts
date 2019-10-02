import { StakePool } from '../../graphql_types'
import { slot1Epoch1 } from '.'

export const stakePool1 = {
  controlledStake: 33,
  createdAt: '2019-08-19T21:27:33',
  description: 'totam suscipit est',
  id: 'h5khccud3aoj7g2fdsdlb5ogixr3ej59lhc39qk20qpk5y3uoh142mx5g2rjm56h',
  isCharity: true,
  ownStake: 25,
  performance: 12,
  profitMargin: 10,
  name: 'Sedrick Wilderman',
  ranking: 1,
  retiring: false,
  rewardPayouts: [{
    slot: slot1Epoch1,
    amount: 1000
  }],
  ticker: 'UIYU',
  url: 'https://linnea.name'
} as StakePool

export const stakePool2 = {
  createdAt: '2019-08-20T01:34:17',
  controlledStake: 55,
  description: 'velit accusantium minima',
  id: 'h5khccud3aoj7g2fdsdlb5ogixr3ej59lhc39qk20qpk5y3uoh142mx5g2rjm56h',
  isCharity: false,
  ownStake: 25,
  performance: 74,
  profitMargin: 44,
  name: 'Rasheed Hartmann',
  ranking: 2,
  retiring: false,
  rewardPayouts: [{
    slot: slot1Epoch1,
    amount: '1000'
  }],
  slotsElected: [{
    epoch: {
      number: 2
    },
    number: 1
  }],
  ticker: 'XGQE',
  url: 'http://maryam.name'
} as StakePool
