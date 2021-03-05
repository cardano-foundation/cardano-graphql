import { exec } from 'child_process'
import { Genesis, ShelleyProtocolParams } from './graphql_types'
import { Config } from './Config'
import { knownEras, capitalizeFirstChar } from '@cardano-graphql/util'

export interface CardanoCliTip {
  blockNo: number,
  headerHash: string, slotNo: number
}

export type ProtocolParams = ShelleyProtocolParams

const isEraMismatch = (errorMessage: string, era: string): boolean => {
  return errorMessage.includes('EraMismatch') ||
    errorMessage.includes(
      `The attempted local state query does not support the ${capitalizeFirstChar(era)} protocol`
    )
}

export interface CardanoCli {
  getProtocolParams(): Promise<ProtocolParams>,
  getTip(): Promise<CardanoCliTip>,
  submitTransaction(filePath: string): Promise<void>
}

export function createCardanoCli (
  cardanoCliPath: Config['cardanoCliPath'],
  genesis: Genesis['shelley'],
  jqPath: Config['jqPath']
): CardanoCli {
  const networkArg = genesis.networkId === '1'
    ? '--mainnet'
    : `--testnet-magic ${genesis.networkMagic.toString()}`
  const query = <T> (
    queryName: string,
    options?: {
      jqOperation?: string,
      withEraFlag: boolean
    }): Promise<T> => {
    return new Promise((resolve, reject) => {
      for (const era of knownEras) {
        const command = [cardanoCliPath, 'query', queryName, networkArg]
        if (options?.withEraFlag) {
          command.push(`--${era}-era`)
        }
        if (options?.jqOperation) {
          command.push('|', jqPath, options?.jqOperation)
        }
        exec(command.join(' '),
          (error, stdout, stderr) => {
            if (error !== null || stderr.toString() !== '') {
              if (!isEraMismatch(stderr.toString(), era)) {
                return reject(new Error(stderr.toString()))
              }
            } else {
              return resolve(JSON.parse(stdout))
            }
          }
        )
      }
    })
  }
  return {
    getProtocolParams: () => query<ProtocolParams>(
      'protocol-parameters',
      {
        withEraFlag: true
      }
    ),
    getTip: () => query<CardanoCliTip>('tip'),
    submitTransaction: (filePath) => {
      return new Promise((resolve, reject) => {
        exec(
          `${cardanoCliPath} transaction submit --tx-file ${filePath} ${networkArg}`,
          (error, _stdout, stderr) => {
            if (error !== null || stderr.toString() !== '') {
              return reject(new Error(stderr.toString()))
            }
            return resolve()
          }
        )
      })
    }
  }
}
