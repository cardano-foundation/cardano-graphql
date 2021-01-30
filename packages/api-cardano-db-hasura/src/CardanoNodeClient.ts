import { CardanoCli } from './CardanoCli'
import fs from 'fs-extra'
import { AssetSupply } from './graphql_types'
import pRetry from 'p-retry'
import util, { DataFetcher } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'

export type LedgerState = {
  accountState: {
    _reserves: string
    _treasury: string
  },
  esNonMyopic: {
    rewardPot: string
  }
}

export class CardanoNodeClient {
  readonly networkParams: string[]
  public adaCirculatingSupply: AssetSupply['circulating']
  public ledgerStateFetcher: DataFetcher<LedgerState>

  constructor (
    private cardanoCli: CardanoCli,
    pollingInterval: number,
    readonly lastConfiguredMajorVersion: number,
    private logger: Logger = dummyLogger
  ) {
    this.ledgerStateFetcher = new DataFetcher<LedgerState>(
      'LedgerState',
      this.cardanoCli.getLedgerState,
      pollingInterval,
      this.logger
    )
  }

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
    await this.ledgerStateFetcher.initialize()
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

  public async shutdown () {
    await this.ledgerStateFetcher.shutdown()
  }
}
