// Epoch 1 is the last epoch in the DB,
// so it has not yet ended, hence the null
// endedAt
export const epoch1 = {
  basic: {
    startedAt: '2017-09-28T21:45:51',
    lastBlockTime: '2017-10-01T02:25:51',
    output: '17282903106017760',
    number: 1,
    transactionsCount: '5344'
  },
  aggregated: {
    blocks_aggregate: {
      aggregate: {
        avg: {
          fees: 28103.350131787032,
          size: 881.9561412756984
        },
        count: 9485,
        max: {
          fees: 684456,
          size: 648087
        },
        min: {
          fees: 0,
          size: 631
        }
      }
    },
    number: 1
  }
}
