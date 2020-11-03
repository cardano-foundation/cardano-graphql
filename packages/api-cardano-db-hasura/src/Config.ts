
export interface Config {
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
  hasuraUri: string
}
