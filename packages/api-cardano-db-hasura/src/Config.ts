
export interface Config {
  cardanoCliPath: string,
  cardanoNodeSocketPath: string,
  // Workaround until we can query the node for the current era
  currentEraFirstSlot: number,
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
  pollingIntervalAdaSupply: number
}
