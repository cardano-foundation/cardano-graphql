import * as path from 'path'
import * as fs from 'fs-extra'
import * as simpleGit from 'simple-git/promise'
import { PullResult } from 'simple-git/promise'
import { MetadataMissingForStakePool } from './errors'
import { StakePool } from './graphql_types'

export type Config = {
  localPath: string,
  remoteUri: string
}

export function StakePoolMetadataRepository ({ localPath, remoteUri }: Config) {
  const git = simpleGit()
  return {
    async init(): Promise<PullResult> {
      if(! await fs.pathExists(localPath)) {
        await git.clone(remoteUri, localPath)
      }
      return await git.pull('origin', 'master')
    },
    destroy() {
      return fs.remove(localPath)
    },
    async get(id: string): Promise<Partial<StakePool>> {
      try {
        // Todo: Add runtime validation of JSON
        return await fs.readJson(path.join(localPath, id))
      } catch (error) {
        throw new MetadataMissingForStakePool(id)
      }
    }
  }
}