import { Config } from './Config'
import fs from 'fs-extra'
import path from 'path'

export async function readSecrets (rootDir: string): Promise<Partial<Config['db']>> {
  return {
    database: (await fs.readFile(path.join(rootDir, 'postgres_db'), 'utf8')).toString(),
    password: (await fs.readFile(path.join(rootDir, 'postgres_password'), 'utf8')).toString(),
    user: (await fs.readFile(path.join(rootDir, 'postgres_user'), 'utf8')).toString()
  }
}
