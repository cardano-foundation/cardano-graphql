import axios, { AxiosInstance } from 'axios'
import { errors, RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'
import { AssetMetadata } from './AssetMetadata'
import { Asset } from './graphql_types'
import pRetry from 'p-retry'

const MODULE_NAME = 'MetadataFetchClient'

export class MetadataClient {
  private axiosClient: AxiosInstance
  public state: RunnableModuleState

  constructor (
    private metadataServerUri: string,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.axiosClient = axios.create({
      baseURL: this.metadataServerUri
    })
  }

  private async ensureLocalMetadataServerIsAvailable (): Promise<void> {
    await pRetry(
      async () => {
        try {
          await this.axiosClient.get('/health')
        } catch (error) {
          if (error.code === 'ENOTFOUND') {
            this.logger.info('Waiting for TokenRegistry to be available')
            throw new errors.HostDoesNotExist('metadata server')
          } else if (error.response?.status === 400) { // Needed until TokenRegistry is updated
            this.logger.info('Token Registry is up')
          } else if (error.response?.status !== 404) {
            this.logger.info('Metadata Server unreachable.')
            throw error
          }
        }
      }, {
        factor: 1.5,
        retries: 10
      }
    )
  }

  private async waitForLocalMetadataServerSynced (): Promise<void> {
    await pRetry(
      async () => {
        try {
          const result = await this.axiosClient.get('/health')
          if (!result.data.synced) {
            this.logger.info('Metadata registry is still syncing. This can take up to 15min...')
            throw new Error('')
          }
        } catch (error) {
          if (error.response?.status === 400) {
            this.logger.info('external Registry is up and running') // Needed until TokenRegistry is updated
          } else {
            throw new Error('')
          }
        }
      }, {
        factor: 1.5,
        retries: 1000,
        minTimeout: 60000 // first try after one minute
      }
    )
  }

  public async fetch (assetIds: Asset['assetId'][]): Promise<AssetMetadata[]> {
    if (this.state !== 'initialized') {
      throw new errors.ModuleIsNotInitialized(MODULE_NAME, 'fetch')
    }
    try {
      const response = await this.axiosClient.post('metadata/query', {
        subjects: assetIds,
        properties: [
          'decimals',
          'description',
          'logo',
          'name',
          'ticker',
          'url'
        ]
      })
      return response.data.subjects
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        this.logger.error({ err: error })
      } else {
        throw error
      }
    }
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: MODULE_NAME }, 'Initializing')
    await this.ensureLocalMetadataServerIsAvailable()
    this.logger.info({ module: MODULE_NAME }, 'Metadata Server is up and running. Checking Sync Status.')
    await this.waitForLocalMetadataServerSynced()
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }
}
