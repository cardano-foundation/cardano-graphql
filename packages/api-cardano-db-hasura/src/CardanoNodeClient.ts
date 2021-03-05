import { CardanoCli } from './CardanoCli'
import fs from 'fs-extra'
import { AssetSupply, Transaction } from './graphql_types'
import pRetry from 'p-retry'
import util, { knownEras } from '@cardano-graphql/util'
import tempWrite from 'temp-write'
import { dummyLogger, Logger } from 'ts-log'
import { getHashOfSignedTransaction } from './util'

const fileTypeFromEra = (era: string) => {
  switch (era) {
    case 'mary' :
      return 'Tx MaryEra'
    case 'allegra' :
      return 'Tx AllegraEra'
    case 'shelley' :
      return 'TxSignedShelley'
    default :
      throw new Error(`Transaction not submitted. ${era} era not supported.`)
  }
}

const isEraMismatch = (errorMessage: string): boolean =>
  errorMessage.includes('DecoderErrorDeserialiseFailure') ||
  errorMessage.includes('The era of the node and the tx do not match')

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']

  constructor (
    private cardanoCli: CardanoCli,
    readonly lastConfiguredMajorVersion: number,
    private logger: Logger = dummyLogger
  ) {}

  public async getTip () {
    const tip = await this.cardanoCli.getTip()
    this.logger.debug('getTip', { module: 'CardanoNodeClient', value: tip })
    return tip
  }

  public async getProtocolParams () {
    const protocolParams = await this.cardanoCli.getProtocolParams()
    this.logger.debug('getProtocolParams', { module: 'CardanoNodeClient', value: protocolParams })
    return protocolParams
  }

  public async initialize () {
    await pRetry(async () => {
      await fs.stat(process.env.CARDANO_NODE_SOCKET_PATH)
      if (!(await this.isInCurrentEra())) {
        this.logger.warn('cardano-node is still synchronizing', { module: 'CardanoNodeClient' })
        throw new Error()
      }
    }, {
      factor: 1.5,
      retries: 39,
      onFailedAttempt: util.onFailedAttemptFor(
        'Establishing connection to cardano-node and ensuring state is in the expected era',
        this.logger
      )
    })
  }

  public async isInCurrentEra () {
    const { protocolVersion } = await this.getProtocolParams()
    this.logger.debug('Comparing current protocol params with last known major version from cardano-node config', {
      module: 'CardanoNodeClient',
      value: {
        currentProtocolVersion: protocolVersion,
        lastConfiguredMajorVersion: this.lastConfiguredMajorVersion
      }
    })
    return protocolVersion.major >= this.lastConfiguredMajorVersion
  }

  public async submitTransaction (transaction: string): Promise<Transaction['hash']> {
    for (const era of knownEras) {
      const filePath = await tempWrite(`{
        "type": "${fileTypeFromEra(era)}",
        "description": "",
        "cborHex": "${transaction}"
      }`)
      const hash = getHashOfSignedTransaction(transaction)
      try {
        await this.cardanoCli.submitTransaction(filePath)
        this.logger.info('submitTransaction', { module: 'CardanoNodeClient', hash: hash })
        return hash
      } catch (error) {
        if (!isEraMismatch(error.message)) {
          throw error
        }
      } finally {
        await fs.unlink(filePath)
      }
    }
  }
}
