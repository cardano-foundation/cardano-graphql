import { ApolloClient, gql, InMemoryCache, NormalizedCacheObject } from 'apollo-boost'
import { createHttpLink } from 'apollo-link-http'
import fetch from 'cross-fetch'
import utc from 'dayjs/plugin/utc'
import dayjs from 'dayjs'
import { exec } from 'child_process'
import { clearIntervalAsync, setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'
import path from 'path'
import pRetry from 'p-retry'
import util from '@cardano-graphql/util'

dayjs.extend(utc)

export class Db {
  hasuraClient: ApolloClient<NormalizedCacheObject>
  hasuraUri: string
  monitorTimer: SetIntervalAsyncTimer

  constructor (hasuraUri: string) {
    this.hasuraUri = hasuraUri
    this.hasuraClient = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false
      }),
      defaultOptions: {
        query: {
          fetchPolicy: 'network-only'
        }
      },
      link: createHttpLink({
        uri: `${this.hasuraUri}/v1/graphql`,
        fetch,
        headers: {
          'X-Hasura-Role': 'cardano-graphql'
        }
      })
    })
  }

  public async init (): Promise<void> {
    // Todo: optimal solution dependent on https://github.com/input-output-hk/cardano-db-sync/issues/182
    await this.applySchemaAndMetadata()
    this.monitorDbState()
  }

  public async shutdown () {
    await clearIntervalAsync(this.monitorTimer)
  }

  private monitorDbState () {
    this.monitorTimer = setIntervalAsync(
      async () => {
        try {
          await this.getMeta()
        } catch (error) {
          if (error.message === 'GraphQL error: field "cardano" not found in type: \'query_root\'' || error.message === 'GraphQL error: database query error') {
            console.warn('Re-applying PostgreSQL migrations and Hasura metadata')
            await this.applySchemaAndMetadata()
          } else {
            console.error(error)
          }
        }
      },
      10000
    )
  }

  async getMeta () {
    const result = await this.hasuraClient.query({
      query: gql`query {
          cardano {
              tip {
                  forgedAt
              }
          }}`
    })
    const { tip } = result.data?.cardano[0]
    const currentUtc = dayjs().utc()
    const tipUtc = dayjs.utc(tip.forgedAt)
    return {
      initialized: tipUtc.isAfter(currentUtc.subtract(120, 'second')),
      syncPercentage: (tipUtc.valueOf() / currentUtc.valueOf()) * 100
    }
  }

  public async applySchemaAndMetadata () {
    await pRetry(async () => {
      await this.hasuraCli('migrate apply --down all')
      await this.hasuraCli('migrate apply --up all')
      await this.hasuraCli('metadata clear')
      await this.hasuraCli('metadata apply')
    }, {
      factor: 1.75,
      retries: 9,
      onFailedAttempt: util.onFailedAttemptFor('Applying PostgreSQL schema and Hasura metadata')
    })
  }

  private hasuraCli (command: string) {
    return new Promise((resolve, reject) => {
      exec(
        `hasura --skip-update-check --project ${path.resolve(__dirname, '..', 'hasura', 'project')} --endpoint ${this.hasuraUri} ${command}`,
        (error, stdout) => {
          if (error) {
            reject(error)
          }
          console.log(stdout)
          resolve()
        }
      )
    })
  }
}
