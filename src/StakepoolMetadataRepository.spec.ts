import * as path from 'path'
import * as fs from 'fs-extra'
import * as simpleGit from 'simple-git/promise'
import { extract } from 'tar'
import { StakePool } from './graphql_types'
import { StakePoolMetadataRepository } from './StakePoolMetadataRepository'

const REPO_NAME = 'stake-pool-metadata'
const repoTar = path.join(path.join(__dirname, '../test/stake-pool-metadata-repo.tar.gz'))
const tempDir = path.join(__dirname, '../test/__temp__/')
const remoteRepoPath = path.join(tempDir, REPO_NAME)
const localPath = path.join(__dirname, '../state', REPO_NAME)
const git = simpleGit()

describe('StakePoolMetadataRepository', () => {
  let metadataRepository: ReturnType<typeof StakePoolMetadataRepository>
  const localRepoExists = () => fs.pathExists(localPath)

  beforeAll(() => fs.ensureDir(tempDir))
  afterAll(() => fs.remove(tempDir))

  beforeEach(async () => {
    if (await fs.pathExists(remoteRepoPath)) await fs.remove(remoteRepoPath)
    await extract({
      file: repoTar,
      C: tempDir
    })
    metadataRepository = StakePoolMetadataRepository({
      cloneOptions: { '--local': null },
      localPath,
      remoteUri: remoteRepoPath
    })
    await metadataRepository.destroy()
  })

  afterEach(async () => {
    await metadataRepository.destroy()
    await fs.remove(remoteRepoPath)
  })

  describe('destroy', () => {
    beforeEach(async () => {
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
    })

    it('cleanups up the local git directory, and is idempotent', async () => {
      await metadataRepository.destroy()
      expect(await localRepoExists()).toBe(false)
      await metadataRepository.destroy()
      expect(await localRepoExists()).toBe(false)
    })
  })

  describe('init', () => {
    beforeEach(async () => {
      expect(await localRepoExists()).toBe(false)
    })

    it('clones the remote repository if no local repository exists, and loads the stake pool entries into the db', async () => {
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
      expect(metadataRepository.size()).toBe(3)
    })
    it('pulls any changes if there is an existing local repository', async () => {
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
      expect(await metadataRepository.has('sp4')).toBe(false)
      await addStakePoolToRemoteRepo('sp4')
      await metadataRepository.init()
      expect(await metadataRepository.has('sp4')).toBe(true)
    })
  })

  describe('has', () => {
    beforeEach(() => metadataRepository.init())

    afterEach(() => metadataRepository.destroy())

    it('knows what is included based on ID', async () => {
      expect(await metadataRepository.has('sp1')).toBe(true)
      expect(await metadataRepository.has('sp2')).toBe(true)
      expect(await metadataRepository.has('sp?')).toBe(false)
    })
  })

  describe('get', () => {
    beforeEach(async () => metadataRepository.init())

    afterEach(async () => metadataRepository.destroy())

    it('returns the Stake Pool metadata for the matching stake pool ID', async () => {
      expect(await metadataRepository.get('sp1')).toMatchSnapshot()
    })
    it('returns undefined no match is found', async () => {
      expect(await metadataRepository.get('?')).toBeUndefined()
    })
  })

  describe('syncWithRemote', () => {
    beforeEach(async () => {
      await metadataRepository.init()
      expect(await localRepoExists()).toBe(true)
      expect(await metadataRepository.has('sp3')).toBe(true)
      expect(await metadataRepository.has('sp4')).toBe(false)
    })

    it('Adds new stake pools', async () => {
      await addStakePoolToRemoteRepo('sp4')
      expect(await metadataRepository.has('sp4')).toBe(false)
      await metadataRepository.syncWithRemote()
      expect(await metadataRepository.has('sp4')).toBe(true)
    })
    it('Updates changes to existing stake pools', async () => {
      expect((await metadataRepository.get('sp3')).profitMargin).toBe(30)
      await updateStakePoolInRemoteRepo('sp3', { profitMargin: 40 })
      await metadataRepository.syncWithRemote()
      expect((await metadataRepository.get('sp3')).profitMargin).toBe(40)
    })
    it('Removes stake pools', async () => {
      await deleteStakePoolInRemoteRepo('sp3')
      expect(await metadataRepository.has('sp3')).toBe(true)
      await metadataRepository.syncWithRemote()
      expect(await metadataRepository.has('sp3')).toBe(false)
    })
  })
})

async function addStakePoolToRemoteRepo (id: string) {
  await fs.writeJson(path.join(remoteRepoPath, `${id}.json`), {
    description: `A stakepool with the ID of ${id}`,
    isCharity: false,
    profitMargin: 30,
    name: `Stake Pool ${id}`,
    ticker: `${id}`,
    url: `http://stake.pool/${id}`
  })
  return commit(`Add stake pool ${id}`)
}

async function updateStakePoolInRemoteRepo (id: string, change: Partial<StakePool>) {
  const filePath = path.join(remoteRepoPath, `${id}.json`)
  await fs.writeJson(filePath, { ...await fs.readJson(filePath), ...change })
  return commit(`Update stake pool ${id}`)
}

async function deleteStakePoolInRemoteRepo (id: string) {
  await fs.unlink(path.join(remoteRepoPath, `${id}.json`))
  return commit(`Delete stake pool ${id}`)
}

async function commit (message: string) {
  await git.cwd(remoteRepoPath)
  await git.add('./*')
  return git.commit(message)
}
