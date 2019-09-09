import * as faker from 'faker'
import { StakePool } from '../../graphql_types'
import { generateSlotWihRewards } from '../data_assertions'
import times = require('lodash.times')

export function generateStakepools (number: number): StakePool[] {
  return times(number, (i) => ({
    controlledStake: faker.random.number(500),
    createdAt: faker.date.past(),
    description: faker.lorem.paragraph(),
    id: faker.finance.bitcoinAddress(),
    isCharity: faker.random.boolean(),
    ownStake: faker.random.number(500),
    performance: faker.random.number(100),
    profitMargin: faker.random.number(20),
    name: faker.company.companyName(),
    ranking: i,
    retiring: false, // Todo: should be Epoch not Boolean
    slotsElected: times(3, generateSlotWihRewards),
    ticker: faker.random.alphaNumeric(4).toUpperCase(),
    url: faker.internet.domainName()
  }))
}
