import { Block } from '@src/graphql_types'

export const block29021 = {
  basic: {
    epoch: {
      number: 1
    },
    epochNo: 1,
    fees: 342316,
    hash: '00aea2405f7cf4eb1d9e6694770ea25eeb113b857e377d22641aa1099742615d',
    merkelRoot: '1acc618aebd703ab201ef790a379a0bd8748ac7a4ed4bf425e0b12f8b7afd812',
    forgedAt: '2017-09-30T15:03:11Z',
    createdBy: 'SlotLeader-8e8a7b0f4a23f07a',
    number: 29021,
    slotInEpoch: 7435,
    slotNo: 29035,
    previousBlock: {
      hash: '663c25ee1434e6d29f8bd08bd52ddbb16a30bb54ca3c69c815e3de7bc4c42770',
      number: 29020
    },
    nextBlock: {
      hash: 'bf13d9a80ad99a4f34edb8a3262dd8120e29bbe182732cd3b00bf3d1bb7c2380',
      number: 29022
    },
    size: 1349,
    transactionsCount: '2'
  },
  aggregated: {
    number: 29021,
    transactions_aggregate: {
      aggregate: {
        avg: {
          fee: 171158,
          size: 218,
          totalOutput: 2846601153795
        },
        count: '2',
        max: {
          fee: '171246',
          size: '220',
          totalOutput: '4924799478660'
        },
        min: {
          fee: '171070',
          size: '216',
          totalOutput: '768402828930'
        },
        sum: {
          deposit: '0',
          fee: '342316',
          size: '436',
          totalOutput: '5693202307590'
        }
      }
    }
  },
  aggregated_filtered: {
    number: 29021,
    transactions_aggregate: {
      aggregate: {
        count: '2'
      }
    }
  }
}

export const block29022 = {
  basic: {
    epoch: {
      number: 1
    },
    epochNo: 1,
    fees: 0,
    hash: 'bf13d9a80ad99a4f34edb8a3262dd8120e29bbe182732cd3b00bf3d1bb7c2380',
    merkelRoot: '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8',
    number: 29022,
    forgedAt: '2017-09-30T15:03:31Z',
    createdBy: 'SlotLeader-7b00350597671036',
    previousBlock: {
      hash: '00aea2405f7cf4eb1d9e6694770ea25eeb113b857e377d22641aa1099742615d',
      number: 29021
    },
    nextBlock: {
      hash: '9a20077183a044fbe41a6ac71747b3463966ba724cd0478786a3df5000b37008',
      number: 29023
    },
    size: 631,
    slotInEpoch: 7436,
    slotNo: 29036,
    transactionsCount: '0'
  }
}

export const block4490600 = {
  basic: {
    epoch: {
      number: 208
    },
    epochNo: 208,
    fees: 3700733,
    hash: 'ab1df510b2fc22a2731212937f532894c474ddccf6240047b38b4a3c14c1895c',
    number: 4490510,
    forgedAt: '2020-07-29T22:14:51Z',
    previousBlock: {
      hash: '02d698482b56b7345e3e0cb8938f6b22f12d1370231457eaa0d991bea3327f5c',
      number: 4490599
    },
    nextBlock: {
      hash: '9fb51dab84fa0e9ae97c6ee48af4e655a330e25d7b96a236d8684761f54eca75',
      number: 4490601
    },
    size: 14476,
    slotInEpoch: 1800,
    slotNo: 4494600,
    transactionsCount: '17',
    vrfKey: 'vrf_vk1ytuuajlv7wvxj57qr6j5ycx69l54ks5nzswqwyn4fhhec6dtdjgsqmzku7'
  } as Partial<Block>
}
