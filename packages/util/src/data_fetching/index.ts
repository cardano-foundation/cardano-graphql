export * from './DataFetcher'
export type FetchFunction<DataValue> = () => Promise<DataValue>
