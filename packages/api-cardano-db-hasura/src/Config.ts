export interface Config {
  cardanoNodeConfigPath: string,
  cardanoNodePrometheus: {
    host: string
    port: number
  },
  hasuraUri: string,
  ogmios?: {
    host?: string
    port?: number
  },
  pollingInterval: {
    adaSupply: number
  }
}
