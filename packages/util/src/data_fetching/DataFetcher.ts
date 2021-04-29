import { FetchFunction } from './index'
import { clearIntervalAsync, setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'
import { dummyLogger, Logger } from 'ts-log'

export class DataFetcher<DataValue> {
  private pollingQueryTimer: SetIntervalAsyncTimer
  private fetch: () => Promise<void>
  private isFetching: boolean
  public value: DataValue

  constructor (
    public name: string,
    fetchFn: FetchFunction<DataValue>,
    private pollingInterval: number,
    private logger: Logger = dummyLogger
  ) {
    this.isFetching = false
    this.fetch = async () => {
      if (this.isFetching) return
      this.isFetching = true
      this.value = await fetchFn()
      this.isFetching = false
    }
  }

  public async initialize () {
    await this.fetch()
    this.logger.debug({ module: 'DataFetcher', instance: this.name, value: this.value }, 'Initial value fetched')
    this.pollingQueryTimer = setIntervalAsync(async () => {
      await this.fetch()
      this.logger.debug({ module: 'DataFetcher', instance: this.name, value: this.value }, `value fetched after ${this.pollingInterval / 1000} seconds`)
    }, this.pollingInterval)
  }

  public shutdown () {
    this.logger.debug(`DataFetcher: ${this.name}: shutdown`)
    return clearIntervalAsync(this.pollingQueryTimer)
  }
}
