import * as faker from 'faker'
import times = require('lodash.times')
import { Address, AddressType, Currency } from '../../graphql_types'

export function generateAddresses (number: number): Address[] {
  return times(number, () => ({
    addressType: AddressType.Base,
    hash: faker.finance.bitcoinAddress(),
    totalValue: {
      currency: Currency.Ada,
      amount: faker.random.number()
    }
  }))
}
