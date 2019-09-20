import * as path from 'path'
import * as fs from 'fs-extra'
import * as simpleGit from 'simple-git/promise'
import { StakePoolMetadataRepository } from './StakePoolMetadataRepository'

const git = simpleGit()
async function addStakePoolToRemoteRepo (id: string, remoteUri: string) {
  await fs.writeJson(`sp_${id}.json`, {
    description: `A stakepool with the ID of ${id}`,
    isCharity: true,
    profitMargin: 30,
    name: `Stake Pool ${id}`,
    ticker: `SP${id}`,
    url: `http://stake.pool/${id}`
  })
  await git.add('./*')
  await git.commit(`Add stake pool ${id}`)
  await git.addRemote('origin', remoteUri)
  return git.push()
}

describe('StakePoolMetadataRepository', () => {
  let metadataRepository: ReturnType<typeof StakePoolMetadataRepository>
  const LOCAL_REPO_PATH = path.join(__dirname, '__temp_metadata_repo__')
  const REMOTE_REPO_URI = 'http://localhost:4040/stake-pool-metadata.git'

  const localRepoExists = () =>  fs.pathExists(LOCAL_REPO_PATH)

  beforeEach(async () => {
    metadataRepository = StakePoolMetadataRepository({
      localPath: LOCAL_REPO_PATH,
      remoteUri: REMOTE_REPO_URI
    })
   await metadataRepository.destroy()
  })

  afterEach(async () => await metadataRepository.destroy())

  describe('init', () => {

    it('Ensures the directory exists and clones the remote repository if needed', async () => {
      expect(await localRepoExists()).toBe(false)
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
      expect(await fs.pathExists(path.join(LOCAL_REPO_PATH, 'stake-pool-metadata','.git')))
    })
    it('pulls any changes if the local repository already exists', async () => {
      expect(await localRepoExists()).toBe(false)
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
      await metadataRepository.init()
      await addStakePoolToRemoteRepo("4", REMOTE_REPO_URI)
      expect(await fs.readFile(path.join(LOCAL_REPO_PATH, 'stake-pool-metadata','sp_4.json')))
    })
  })

  // describe('get', () => {
  //
  //   beforeEach(() => {
  //     metadataRepository.init()
  //   })
  //
  //   it('returns the json for the matching stake pool ID', () => {
  //
  //   })
  //   it('throws a 404 if no match is found', () => {
  //
  //   })
  // })

})
