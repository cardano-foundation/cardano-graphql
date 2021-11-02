import { Block } from '@src/graphql_types'

export const block3037760 = {
  basic: {
    epoch: {
      number: 166
    },
    epochNo: 166,
    fees: 3874838,
    hash: 'd3bdf2725bf90e7048a908d2045fa92885d7baaf9fd60c530f7d758c8549b928',
    forgedAt: '2021-11-01T12:20:17',
    number: 3037760,
    slotInEpoch: 57601,
    slotNo: 41400001,
    previousBlock: {
      hash: 'eb3a492a1d9b6ff6f5ce0ed4745e032f9ddbca793150cf881cb503a7ffd4a15e',
      number: 3037759
    },
    nextBlock: {
      hash: '26e5bae89d6eaadacbba338bdf472ee7a2e71858a1048d8f47aa69d4c2abb3f3',
      number: 3037761
    },
    size: 6858,
    transactionsCount: '22',
    vrfKey: 'vrf_vk1pjw0hnkc3lw8yvwx0fxy9wqhtekphzy0uaqpfr625rmrzgjfvkeqcdkkdh'
  },
  aggregated: {
    number: 3037760,
    transactions_aggregate: {
      aggregate: {
        avg: {
          fee: 176129,
          size: 313.27272727272725,
          totalOutput: 421674965437.5455
        },
        count: '22',
        max: {
          fee: '204045',
          size: '858',
          totalOutput: '9223859752216'
        },
        min: {
          fee: '167085',
          size: '237',
          totalOutput: '12806911'
        },
        sum: {
          deposit: '0',
          fee: '3874838',
          size: '6892',
          totalOutput: '9276849239626'
        }
      }
    }
  },
  aggregated_filtered: {
    number: 3037760,
    transactions_aggregate: {
      aggregate: {
        count: '21'
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
    hash: '36f4d3e433e54c43d2a13e22b6b7168dcf672d446b14f1fcc725eaf13d8c102b',
    number: 29022,
    forgedAt: '2019-07-31T19:17:36',
    previousBlock: {
      hash: '2f74e2f742535c8cd1a11e31524cff2e503fa3b667a82e79e377e3f8c160a3af',
      number: 29021
    },
    nextBlock: {
      hash: '7063aa69f0923b1d603743644e40140021dfe0561630a1e2badb6660cc474c0e',
      number: 29023
    },
    size: 631,
    slotInEpoch: 8452,
    slotNo: 30052,
    transactionsCount: '0'
  }
}

export const block2490600 = {
  basic: {
    epoch: {
      number: 125
    },
    epochNo: 125,
    fees: 0,
    hash: '94f99e9dadd818a49d7966f788bbe1a7b31fd109217ffc015d450db9b262aa90',
    number: 2490600,
    forgedAt: '2021-04-13T21:49:47',
    previousBlock: {
      hash: 'c852351dea5905e1950a6cb63bae306f64d0e68dbd3317178f14f1413735e84c',
      number: 2490599
    },
    nextBlock: {
      hash: 'def09fc71da9f739b3a414896391cc33ff1e9b9688a4f37e8e5538247571651d',
      number: 2490601
    },
    size: 3,
    slotInEpoch: 350971,
    slotNo: 23981371,
    transactionsCount: '0',
    vrfKey: 'vrf_vk16t90lzn5arxkz5det5yzwty0ja9vq6weupade2gfp8nc4gy3llrsd6nkwe'
  } as Partial<Block>
}
