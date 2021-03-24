import { Epoch } from '@src/graphql_types'

export const epoch1 = {
  basic: {
    adaPots: null,
    startedAt: '2017-09-28T21:44:51Z',
    blocksCount: '21590',
    fees: 1033002678,
    lastBlockTime: '2017-10-03T21:44:31Z',
    output: '101402912214214220',
    number: 1,
    transactionsCount: '12870'
  } as Epoch,
  aggregated: {
    blocksCount: '21590',
    activeStake_aggregate: {
      aggregate: {
        sum: {
          amount: null
        }
      }
    } as Epoch['activeStake_aggregate'],
    blocks_aggregate: {
      aggregate: {
        avg: {
          fees: 47846.34914312181,
          size: 868.0383047707272
        },
        count: '21590',
        max: {
          fees: '1377042',
          size: '648087'
        },
        min: {
          fees: '0',
          size: '631'
        },
        sum: {
          fees: '1033002678',
          size: '18740947'
        }
      }
    },
    fees: 1033002678,
    number: 1
  }
}

export const epoch220 = {
  basic: {
    adaPots: {
      deposits: '739776000000',
      fees: '4970194949',
      reserves: '13120582265809832',
      rewards: '151012138061367',
      slotNo: 9676800,
      treasury: '94812346026398',
      utxo: '31632848503907452'
    },
    startedAt: '2020-09-27T21:44:51Z',
    blocksCount: '21627',
    fees: 5135934788,
    lastBlockTime: '2020-10-02T21:44:19Z',
    output: '12708523642248888',
    number: 220,
    transactionsCount: '19534'
  } as Epoch,
  protocolParams: {
    a0: 0.3,
    decentralisationParam: 0.64,
    eMax: 18,
    extraEntropy: null as any,
    keyDeposit: 2000000,
    maxBlockBodySize: 65536,
    maxBlockHeaderSize: 1100,
    maxTxSize: 16384,
    minFeeA: 44,
    minFeeB: 155381,
    minPoolCost: 340000000,
    minUTxOValue: 1000000,
    nOpt: 150,
    poolDeposit: 500000000,
    protocolVersion: {
      major: 2,
      minor: 2
    },
    rho: 0.003,
    tau: 0.2
  },
  aggregated: {
    activeStake_aggregate: {
      aggregate: {
        sum: {
          amount: '15859988643309526'
        }
      }
    },
    blocksCount: '21627',
    blocks_aggregate: {
      aggregate: {
        avg: {
          fees: 237477.9113145605,
          size: 491.35959680029595
        },
        count: '21627',
        max: {
          fees: '498861647',
          size: '19292'
        },
        min: {
          fees: '0',
          size: '3'
        },
        sum: {
          fees: '5135934788',
          size: '10626634'
        }
      }
    },
    fees: 5135934788,
    number: 220
  }
}
