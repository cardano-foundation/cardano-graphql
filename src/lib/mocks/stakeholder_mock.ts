import * as faker from 'faker'
import { StakeDelegation, Stakeholder } from '../../graphql_types'
import { generateStakepools } from './stakepool_mock'
import times = require('lodash.times')

export function generateStakeDelegations (number: number): StakeDelegation[] {
  return times(number, () => ({
    rewardAccount: {
      address: faker.finance.bitcoinAddress(),
      amount: faker.random.number()
    },
    stakingKey: faker.finance.bitcoinAddress(),
    stakePool: generateStakepools(1)[0]
  }))
}

export function generateStakeholders (number: number): Stakeholder[] {
  const delegations = generateStakeDelegations(faker.random.number(10))
  return times(number, () => ({
    delegations,
    totalRewardBalance: Object.values(delegations)
      .reduce((prev, { rewardAccount }) => prev + rewardAccount.amount, 0)
  }))
}
