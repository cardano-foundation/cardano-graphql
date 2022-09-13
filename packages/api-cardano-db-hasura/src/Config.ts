export interface Config {
  cardanoNodeConfigPath: string,
  db?: {
    database?: string,
    host?: string,
    password?: string,
    port?: number
    user?: string,
  },
  hasuraUri: string,
  metadataServerUri: string,
  metadataUpdateInterval?: {
    assets: number
  },
  ogmios?: {
    host?: string
    port?: number
  },
  pollingInterval: {
    adaSupply: number
  }
}
