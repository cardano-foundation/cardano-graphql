export interface Config {
  cardanoNodeConfigPath: string,
  hasuraUri: string,
  ogmios?: {
    host?: string
    port?: number
  },
  pollingInterval: {
    adaSupply: number
  }
}
