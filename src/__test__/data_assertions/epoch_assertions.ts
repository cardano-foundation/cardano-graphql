// Epoch 1 is the last epoch in the DB,
// so it has not yet ended, hence the null
// endedAt
export const epoch1 = {
  basic: {
    startedAt: '2017-09-28T21:45:51',
    lastBlockTime: '2017-10-03T21:43:51',
    output: '101402912214214220',
    number: 1,
    transactionsCount: '12870'
  },
  aggregated: {
    blocks_aggregate: {
      aggregate: {
        avg: {
          fees: 47846.34914312181,
          size: 868.0383047707272
        },
        count: 21590,
        max: {
          fees: 1377042,
          size: 648087
        },
        min: {
          fees: 0,
          size: 631
        },
        sum: {
          fees: 1033002678,
          size: 18740947
        }
      }
    },
    number: 1
  }
}
