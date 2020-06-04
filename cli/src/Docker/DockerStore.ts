import fs from 'fs-extra'
import path from 'path'
import { dummyLogger, Logger } from 'ts-log'
import Conf from 'conf'

export type DockerState = {
  compose?: {
    createdAt: number
  }
  managedImages?: string[],
  secrets: {
    createdAt: number
  }
  workingDirectory: string
}

export class DockerStore {
  private logger: Logger

  private conf: Conf

  public constructor (logger: Logger) {
    this.conf = new Conf({
      ...process.env.NODE_ENV === 'test' ? { projectName: 'cardano-graphql-test' } : {},
      configName: 'docker',
      schema: {
        compose: {
          type: 'object',
          properties: {
            createdAt: {
              type: 'number'
            }
          }
        },
        managedImages: {
          type: 'array',
          items: {
            type: 'string'
          }
        },
        secrets: {
          type: 'object',
          properties: {
            createdAt: {
              type: 'number'
            }
          }
        },
        workingDirectory: {
          type: 'string'
        }
      }
    })
    this.logger = logger || dummyLogger
  }

  public async createSecrets (password: string, user: string): Promise<void> {
    await fs.ensureDir(this.secretsDir)
    await Promise.all([
      fs.writeFile(path.join(this.secretsDir, 'postgres_db'), 'cexplorer'),
      fs.writeFile(path.join(this.secretsDir, 'postgres_password'), password),
      fs.writeFile(path.join(this.secretsDir, 'postgres_user'), user)
    ])
    await this.set('secrets', { createdAt: Date.now() })
    this.logger.info('Secrets created')
  }

  public async cleanup (): Promise<void> {
    try {
      const dirStat = await fs.stat(this.secretsDir)
      if (dirStat.isDirectory()) {
        await fs.remove(this.localConfigDir)
        this.logger.info('Secrets removed')
      }
      if (this.get('compose') !== undefined) {
        const fileStat = await fs.stat(this.dockerComposeFilePath)
        if (fileStat.isFile()) {
          await fs.remove(this.dockerComposeFilePath)
          this.logger.info('docker-compose.yml removed')
        }
      }
    } catch (error) {
      this.logger.debug('No secrets to remove')
    } finally {
      await fs.remove(path.dirname(this.conf.path))
      this.logger.info('Config removed')
    }
  }

  public get (prop: string): any {
    return this.conf.get(prop)
  }

  public set (prop: string, value: any): any {
    return this.conf.set(prop, value)
  }

  private get secretsDir () {
    return path.join(this.get('workingDirectory'), 'config', 'secrets')
  }

  public get dockerComposeFilePath () {
    return path.join(this.get('workingDirectory'), 'docker-compose.yml')
  }

  private get configDir () {
    this.logger.debug(this.conf.path)
    return path.dirname(this.conf.path)
  }

  private get localConfigDir () {
    return path.join(this.get('workingDirectory'), 'config')
  }

  // Todo: move to envPaths.data https://github.com/sindresorhus/env-paths#pathsconfig
  public get snapshotsDir () {
    return path.join(this.configDir, 'snapshots')
  }

  public initSnapshot (): Promise<string> {
    const dirPath = path.join(this.snapshotsDir, Date.now().toString())
    return fs.mkdirp(dirPath).then(() => dirPath)
  }

  public async snapshotsList () {
    const files = await fs.readdir(this.snapshotsDir)
    return files.map((fileName) => ({
      name: fileName,
      time: fs.statSync(path.join(this.snapshotsDir, fileName)).mtime.getTime()
    })).sort((a, b) => {
      return b.time - a.time
    }).map((v) => v.name)
  }

  public async validateSnapshot (snapshot: string): Promise<void> {
    const fileStat = await fs.stat(path.join(this.snapshotsDir, snapshot))
    if (!fileStat.isDirectory()) throw new Error('Invalid snapshot')
  }
}
