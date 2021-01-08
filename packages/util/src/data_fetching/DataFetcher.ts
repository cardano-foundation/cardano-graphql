import { FetchFunction } from './index'
import { clearIntervalAsync, setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'
import { dummyLogger, Logger } from 'ts-log'

export class DataFetcher<DataValue> {
  private pollingQueryTimer: SetIntervalAsyncTimer
  public value: DataValue

  constructor (
    public name: string,
    private fetchFn: FetchFunction<DataValue>,
    private pollingInterval: number,
    private logger: Logger = dummyLogger
  ) {}

  public async initialize () {
    this.value = await this.fetchFn()
    this.logger.debug('initial value fetched', { module: 'DataFetcher', instance: this.name, value: this.value })
    this.pollingQueryTimer = setIntervalAsync(async () => {
      this.value = await this.fetchFn()
      this.logger.debug(`value fetched after ${this.pollingInterval / 1000} seconds`, { module: 'DataFetcher', instance: this.name, value: this.value })
    }, this.pollingInterval)
  }

  public shutdown () {
    this.logger.debug(`DataFetcher: ${this.name}: shutdown`)
    return clearIntervalAsync(this.pollingQueryTimer)
  }
}
