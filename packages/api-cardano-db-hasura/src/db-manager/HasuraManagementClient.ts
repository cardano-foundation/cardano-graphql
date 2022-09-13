import { exec } from 'child_process'
import util, { ModuleState } from '@cardano-graphql/util'
import { GraphQLSchema } from 'graphql'
import { GraphQLClient, gql } from 'graphql-request'
import pRetry from 'p-retry'
import path from 'path'
import { dummyLogger, Logger } from 'ts-log'

const epochInformationNotYetAvailable = 'Epoch information not yet available. This is expected during the initial chain-sync.'

export class HasuraManagementClient {
  private client: GraphQLClient
  private applyingSchemaAndMetadata: boolean
  private state: ModuleState
  public schema: GraphQLSchema

  constructor (
    readonly hasuraCliPath: string,
    readonly hasuraUri: string,
    private logger: Logger = dummyLogger
  ) {
    this.state = null
    this.applyingSchemaAndMetadata = false
    this.client = new GraphQLClient(
      `${this.hasuraUri}/v1/graphql`,
      {
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      }
    )
  }

  private async hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `${this.hasuraCliPath} --skip-update-check --project ${path.resolve(__dirname, '..', '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          if (stdout !== '') this.logger.debug({ module: 'HasuraManagementClient' }, stdout)
          resolve()
        }
      )
    })
  }

  public async initialize () {
    if (this.state !== null) return
    this.state = 'initializing'
    this.logger.info({ module: 'HasuraManagementClient' }, 'Initializing')
    await this.applySchemaAndMetadata()
    this.logger.debug({ module: 'HasuraManagementClient' }, 'graphql-engine setup')
    await pRetry(async () => {
      const result = await this.client.request(
        gql`query {
            epochs (limit: 1, order_by: { number: desc }) {
                number
            }
        }`
      )
      if (result.epochs.length === 0) {
        this.logger.debug({ module: 'HasuraManagementClient' }, epochInformationNotYetAvailable)
        throw new Error(epochInformationNotYetAvailable)
      }
    }, {
      factor: 1.05,
      retries: 100,
      onFailedAttempt: util.onFailedAttemptFor(
        'Detecting DB sync state has reached minimum progress',
        this.logger
      )
    })
    this.logger.debug({ module: 'HasuraManagementClient' }, 'DB sync state has reached minimum progress')
    this.state = 'initialized'
    this.logger.info({ module: 'HasuraManagementClient' }, 'Initialized')
  }

  public async shutdown () {
    this.state = null
  }

  public async applySchemaAndMetadata (): Promise<void> {
    if (this.applyingSchemaAndMetadata) return
    this.applyingSchemaAndMetadata = true
    await pRetry(async () => {
      await this.hasuraCli('migrate apply --down all')
      await this.hasuraCli('migrate apply --up all')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor(
        'Applying PostgreSQL schema migrations',
        this.logger
      )
    })
    await pRetry(async () => {
      await this.hasuraCli('metadata clear')
      await this.hasuraCli('metadata apply')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor('Applying Hasura metadata', this.logger)
    })
    this.applyingSchemaAndMetadata = false
  }
}
