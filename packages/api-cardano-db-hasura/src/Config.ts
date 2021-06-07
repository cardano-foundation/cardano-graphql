
export interface Config {
  cardanoNodeConfigPath: string,
  db: {
    database: string,
    host: string,
    password: string,
    port: number
    user: string,
  },
  genesis: {
    byronPath: string,
    shelleyPath: string
  },
  hasuraCliPath: string,
  hasuraUri: string,
  jqPath: string,
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
