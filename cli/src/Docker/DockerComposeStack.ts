import fs from 'fs-extra'
import path from 'path'
import { down, IDockerComposeResult, ps, stop, upAll } from 'docker-compose'
import { cardanoNodeService, DockerClient, postgresService, StatefulService } from './'
import { dummyLogger, Logger } from 'ts-log'
import { DockerState, DockerStore } from './DockerStore'

enum StackState {
  up = 'up',
  down = 'down',
  stopped = 'stopped'
}

export type StatefulServiceMap = Map<string, StatefulService>

export class DockerComposeStack {
  readonly docker: DockerClient

  readonly logger: Logger

  readonly projectName: string

  private store: DockerStore

  public statefulServices: StatefulServiceMap

  constructor (
    store: DockerStore,
    dockerClient: DockerClient,
    options: {
      logger: Logger,
      projectName?: string,
      statefulServices?: StatefulServiceMap
    }
  ) {
    this.docker = dockerClient
    this.logger = options.logger || dummyLogger
    this.projectName = options.projectName || 'cardano-graphql'
    this.store = store
    this.statefulServices = options.statefulServices || new Map([
      ['postgres', postgresService(this.projectName)],
      ['cardano-node', cardanoNodeService(this.projectName)]
    ])
  }

  public async init (workingDirectory: string): Promise<void> {
    this.store.set('workingDirectory', workingDirectory)
    const compose = await this.store.get('compose') as DockerState['compose']
    if (compose && compose.createdAt !== undefined) {
      this.logger.info('Already initialized')
      return
    }
    // await fs.copyFile(path.resolve(__dirname, '..', '..', 'docker-compose.yml'), this.store.dockerComposeFilePath)
    await fs.copyFile(path.resolve(__dirname, '..', '..', 'src', 'docker-compose.yml'), this.store.dockerComposeFilePath)
    await this.store.set('compose', { createdAt: Date.now() } as DockerState['compose'])
    this.logger.info('docker-compose.yml created')
  }

  private async up (): Promise<IDockerComposeResult> {
    const currentState = await this.currentState()
    if (currentState === StackState.up) return
    this.logger.info('docker-compose up')
    return upAll({
      composeOptions: ['-p', this.projectName]
    })
  }

  private async down (): Promise<void> {
    const currentState = await this.currentState()
    if (currentState === StackState.down) return
    this.logger.info('docker-compose down')
    await down({
      composeOptions: ['-p', this.projectName]
    })
  }

  private async stop (): Promise<void> {
    const currentState = await this.currentState()
    if (currentState === StackState.stopped || currentState === StackState.down) return
    this.logger.info('docker-compose stop')
    await stop({
      composeOptions: ['-p', this.projectName]
    })
  }

  public async makeSnapshot (): Promise<void> {
    const beforeState = await this.currentState()
    await this.stop()
    try {
      await this.docker.makeSnapshot(this.statefulServices)
    } catch (error) {
      this.logger.error(error)
    } finally {
      if (beforeState === StackState.up) {
        await this.up()
      }
      this.logger.info('Snapshot complete')
    }
  }

  public async restoreSnapshot (snapshotName?: string): Promise<void> {
    const snapshot = snapshotName !== undefined ? snapshotName : (await this.store.snapshotsList())[0]
    const allServices = [...this.statefulServices.values()]
    await this.docker.canRestore(snapshot)
    const beforeState = await this.currentState()
    await this.down()
    await this.removeServiceVolumes(allServices)
    await this.createServiceVolumes(allServices)
    await this.docker.restoreSnapshot(snapshot, allServices)
    if (beforeState === StackState.up) {
      await this.up()
    }
    this.logger.info('Snapshot restore complete')
  }

  public async rebuildService (services: StatefulService[], backup = true): Promise<void> {
    if (backup === true) {
      await this.makeSnapshot()
    }
    const beforeState = await this.currentState()
    await this.down()
    try {
      await this.removeServiceVolumes(services)
    } catch (error) {
      this.logger.error(error)
    } finally {
      if (beforeState === StackState.up) {
        await this.up()
      }
    }
  }

  private async currentState (): Promise<StackState> {
    const psResult = await ps({
      commandOptions: ['-q']
    })
    if (psResult.out.length === 0) {
      const stoppedContainers = await ps({
        commandOptions: ['-aq']
      })
      if (stoppedContainers.out.length === 0) {
        return StackState.down
      } else {
        return StackState.stopped
      }
    } else {
      return StackState.up
    }
  }

  private async createServiceVolumes (services: StatefulService[]): Promise<void> {
    for (const service of services) {
      for (const volume of service.volumes) {
        await this.docker.createVolume(volume)
      }
    }
  }

  private async removeServiceVolumes (services: StatefulService[]): Promise<void> {
    try {
      for (const service of services) {
        for (const volume of service.volumes) {
          await this.docker.removeVolume(volume)
        }
      }
    } catch (error) {
      this.logger.warn(error.message)
    }
  }
}
