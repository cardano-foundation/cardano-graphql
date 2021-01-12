import { exec } from 'child_process'
import { Genesis } from './graphql_types'
import { Config } from './Config'
import { LedgerState } from './CardanoNodeClient'

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
  eraName: Config['eraName'],
  genesis: Genesis['shelley'],
  jqPath: Config['jqPath']
): CardanoCli {
  const networkArg = genesis.networkId === '1'
    ? '--mainnet'
    : `--testnet-magic ${genesis.networkMagic.toString()}`
  const eraArg = `--${eraName}-era`
  return {
    getLedgerState: () => {
      return new Promise((resolve, reject) => {
        exec(
          `${cardanoCliPath} query ledger-state ${networkArg} ${eraArg} | 
          ${jqPath} "{accountState: .nesEs.esAccountState, esNonMyopic: { rewardPot: .nesEs.esNonMyopic.rewardPotNM } }"
          `,
          (error, stdout, stderr) => {
            if (error !== null || stderr.toString() !== '') {
              return reject(new Error(stderr.toString()))
            }
            return resolve(JSON.parse(stdout))
          }
        )
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
