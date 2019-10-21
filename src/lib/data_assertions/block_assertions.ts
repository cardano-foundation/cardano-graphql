import { tx05ad8b, txe68043 } from './transaction_assertions'

export const block29021 = {
  epoch: {
    number: 1
  },
  fees: '342316',
  id: '00aea2405f7cf4eb1d9e6694770ea25eeb113b857e377d22641aa1099742615d',
  merkelRootHash: '1acc618aebd703ab201ef790a379a0bd8748ac7a4ed4bf425e0b12f8b7afd812',
  createdAt: '2017-09-30T15:03:11',
  number: 29021,
  slotNo: 29035,
  previousBlock: {
    id: '663c25ee1434e6d29f8bd08bd52ddbb16a30bb54ca3c69c815e3de7bc4c42770',
    number: 29020
  },
  size: 1349,
  transactions: [tx05ad8b, txe68043]
}

export const block29022 = {
  epoch: {
    number: 1
  },
  fees: '0',
  id: 'bf13d9a80ad99a4f34edb8a3262dd8120e29bbe182732cd3b00bf3d1bb7c2380',
  merkelRootHash: '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8',
  number: 29022,
  createdAt: '2017-09-30T15:03:31',
  previousBlock: {
    id: '00aea2405f7cf4eb1d9e6694770ea25eeb113b857e377d22641aa1099742615d',
    number: 29021
  },
  size: 631,
  slotNo: 29036,
  transactions: [] as any
}
