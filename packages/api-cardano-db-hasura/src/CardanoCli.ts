import { exec } from 'child_process'
import { Genesis } from './graphql_types'
import { Config } from './Config'
import { LedgerState } from './CardanoNodeClient'
import { knownEras } from '@cardano-graphql/util'

export interface CardanoCliTip {
  blockNo: number,
  headerHash: string, slotNo: number
}

export interface CardanoCli {
  getLedgerState(): Promise<LedgerState>,
  getTip(): Promise<CardanoCliTip>
}

export function createCardanoCli (
  cardanoCliPath: Config['cardanoCliPath'],
  genesis: Genesis['shelley'],
  jqPath: Config['jqPath']
): CardanoCli {
  const networkArg = genesis.networkId === '1'
    ? '--mainnet'
    : `--testnet-magic ${genesis.networkMagic.toString()}`
  return {
    getLedgerState: () => {
      return new Promise((resolve, reject) => {
        for (const era of knownEras) {
          exec(
            `${cardanoCliPath} query ledger-state ${networkArg} --${era}-era | 
          ${jqPath} "{accountState: .nesEs.esAccountState, esNonMyopic: { rewardPot: .nesEs.esNonMyopic.rewardPotNM } }"
          `,
            (error, stdout, stderr) => {
              if (error !== null || stderr.toString() !== '') {
                if (
                  stderr.toString().match(/EraMismatch/g) !== null ||
                  stderr.toString().match(/The attempted local state query does not support the/g) !== null
                ) {
                } else {
                  reject(new Error(stderr.toString()))
                }
              } else {
                resolve(JSON.parse(stdout))
              }
            }
          )
        }
      })
    },
    getTip: () => {
      return new Promise((resolve, reject) => {
        exec(
          `${cardanoCliPath} query tip ${networkArg}`,
          (error, stdout, stderr) => {
            if (error !== null || stderr.toString() !== '') {
              return reject(new Error(stderr.toString()))
            }
            return resolve(JSON.parse(stdout))
          }
        )
      })
    }
  }
}
