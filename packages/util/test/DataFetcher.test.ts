import delay from 'delay'
import { jest } from '@jest/globals'
import { DataFetcher } from '@src/data_fetching'

const ctx = {
  fetchFunctionResolvesTrue: () => Promise.resolve(true)
}

describe('DataFetcher', () => {
  let dataFetcher: DataFetcher<boolean>
  let fetchFunctionResolvesTrueSpy: ReturnType<typeof jest.spyOn>

  beforeEach(() => {
    fetchFunctionResolvesTrueSpy = jest.spyOn(ctx, 'fetchFunctionResolvesTrue')
  })

  afterEach(() => {
    jest.clearAllMocks()
  })

  describe('initialize', () => {
    beforeEach(() => {
      dataFetcher = new DataFetcher<boolean>(fetchFunctionResolvesTrueSpy, 10)
    })

    afterEach(() => {
      dataFetcher.shutdown()
    })

    it('calls the function immediately, then on each interval tick', async () => {
      expect(fetchFunctionResolvesTrueSpy).toHaveBeenCalledTimes(0)
      await dataFetcher.initialize()
      expect(fetchFunctionResolvesTrueSpy).toHaveBeenCalledTimes(1)
      await delay(22)
      expect(fetchFunctionResolvesTrueSpy).toHaveBeenCalledTimes(3)
    })
  })

  describe('shutdown', () => {
    beforeEach(async () => {
      dataFetcher = new DataFetcher<boolean>(fetchFunctionResolvesTrueSpy, 10)
      await dataFetcher.initialize()
    })

    it('stops the function being called', async () => {
      expect(fetchFunctionResolvesTrueSpy).toHaveBeenCalledTimes(1)
      await dataFetcher.shutdown()
      await delay(11)
      expect(fetchFunctionResolvesTrueSpy).toHaveBeenCalledTimes(1)
    })
  })
})
