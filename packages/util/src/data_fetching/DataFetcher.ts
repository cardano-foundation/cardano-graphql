import { FetchFunction } from './index'
import { clearIntervalAsync, setIntervalAsync, SetIntervalAsyncTimer } from 'set-interval-async/dynamic'

export class DataFetcher<DataValue> {
  private fetchFn: FetchFunction<DataValue>
  private pollingInterval: number
  private pollingQueryTimer: SetIntervalAsyncTimer
  public value: DataValue

  constructor (fetchFn: FetchFunction<DataValue>, pollingInterval: number) {
    this.fetchFn = fetchFn
    this.pollingInterval = pollingInterval
  }

  public async initialize () {
    this.value = await this.fetchFn()
    this.pollingQueryTimer = setIntervalAsync(async () => {
      this.value = await this.fetchFn()
    }, this.pollingInterval)
  }

  public shutdown () {
    return clearIntervalAsync(this.pollingQueryTimer)
  }
}
