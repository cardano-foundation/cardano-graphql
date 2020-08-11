
export interface Config {
  db: {
    database: string,
    host: string,
    password: string,
    port: number
    user: string,
  },
  hasuraUri: string
}
