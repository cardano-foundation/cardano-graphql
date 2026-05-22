export interface Config {
  cardanoNodeConfigPath: string,
  cardanoNodePrometheus: {
    host: string
    port: number
  },
  cardanoSubmitApi: {
    host: string
    port: number
  },
  hasuraUri: string,
  pollingInterval: {
    adaSupply: number
  }
}
