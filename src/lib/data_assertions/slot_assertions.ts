import * as faker from 'faker'
import { Slot } from '../../graphql_types'
import { generateRewardPayout } from '../mocks/slot_mock'
import times = require('lodash.times')

export const slot1Epoch2 = {
  epoch: {
    number: 2
  },
  number: 1
} as Slot

export function generateSlotWihRewards (): Slot {
  return {
    ...slot1Epoch2,
    leader: faker.finance.bitcoinAddress(),
    rewardPayouts: times(10, generateRewardPayout.bind(this, slot1Epoch2))
  }
}
