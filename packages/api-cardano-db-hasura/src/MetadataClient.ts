import axios, { AxiosInstance } from 'axios'
import { errors, RunnableModuleState } from '@cardano-graphql/util'
import { dummyLogger, Logger } from 'ts-log'
import { AssetMetadata } from './AssetMetadata'
import { Asset } from './graphql_types'

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

  private async ensureMetadataServerIsAvailable (): Promise<void> {
    try {
      await this.axiosClient.get('/metadata/healthcheck')
    } catch (error) {
      if (error.code === 'ENOTFOUND') {
        throw new errors.HostDoesNotExist('metadata server')
      } else if (error.response.status !== 404) {
        throw error
      }
    }
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
    await this.ensureMetadataServerIsAvailable()
    this.state = 'initialized'
    this.logger.info({ module: MODULE_NAME }, 'Initialized')
  }
}
