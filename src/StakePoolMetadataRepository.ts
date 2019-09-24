import * as path from 'path'
import * as fs from 'fs-extra'
import * as simpleGit from 'simple-git/promise'
import { StakePool } from './graphql_types'
const lowDb = require('lowdb')
const Memory = require('lowdb/adapters/Memory')

export type Config = {
  cloneOptions?: { [key: string]: null | string }
  localPath: string,
  remoteUri: string
}

export function StakePoolMetadataRepository ({ cloneOptions, localPath, remoteUri }: Config) {
  let git: ReturnType<typeof simpleGit>
  let db: ReturnType<typeof lowDb>
  async function stakePoolData (file: string) {
    const entry = await fs.readJson(path.join(localPath, file))
    return {
      ...entry,
      ...{ id: path.parse(file).name }
    }
  }

  return {
    destroy () {
      return fs.remove(localPath)
    },
    get (id: string): Promise<Partial<StakePool>> {
      return db.get('stakePools').find({ id }).value()
    },
    async has (id: string) {
      return !!await this.get(id)
    },
    async init () {
      db = lowDb(new Memory())
      if (!await fs.pathExists(localPath)) {
        git = simpleGit()
        await git.clone(remoteUri, localPath, cloneOptions)
      } else {
        git = simpleGit(localPath)
        await this.pull()
      }
      const entries = (await fs.readdir(localPath))
        .filter(isMetadataFile)
        .map(stakePoolData)
      db.defaults({ stakePools: await Promise.all(entries) }).write()
    },
    async pull () {
      await git.cwd(localPath)
      return git.pull('origin', 'master')
    },
    size () {
      return db.get('stakePools').size().value()
    },
    async syncWithRemote () {
      const res = await this.pull()
      const { created, deleted, files } = res
      if (created.length > 0) {
        const stakePools = created.filter(isMetadataFile)
        for (const file of stakePools) {
          db.get('stakePools').push(await stakePoolData(file)).write()
        }
      } else if (deleted.length > 0) {
        const stakePools = deleted.filter(isMetadataFile)
        for (const file of stakePools) {
          db.get('stakePools').remove({ id: path.parse(file).name }).write()
        }
      } else if (files.length > 0) {
        const stakePools = files.filter(isMetadataFile)
        for (const file of stakePools) {
          db.get('stakePools').find({ id: path.parse(file).name })
            .assign(await stakePoolData(file)).write()
        }
      }
    }
  }
}

function isMetadataFile (file: string): boolean {
  return !/^\..*/.test(file) && path.extname(file).toLowerCase() === '.json'
}
