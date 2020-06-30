import Docker from 'dockerode'
import path from 'path'
import { dummyLogger, Logger } from 'ts-log'
import { Stream } from 'stream'
import { DockerState, DockerStore } from './DockerStore'
import { StatefulServiceMap } from './DockerComposeStack'
import { StatefulService } from './services'

type ModemProgressEvent = {
  id?: string
  status: string
  progressDetail?: {
    current?: number
    total?: number
  }
  progress?: string
}

export class DockerClient {
  readonly docker: Docker

  readonly logger: Logger

  private store: DockerStore

  readonly utilityImage: string

  constructor (
    store: DockerStore,
    options: {
      logger: Logger,
      utilityImage?: string
    }) {
    this.docker = new Docker()
    this.logger = options.logger || dummyLogger
    this.store = store
    this.utilityImage = options.utilityImage || 'alpine:3.12.0'
  }

  public async ensureUtilityImageAvailable () {
    if (await this.hasImage(this.utilityImage) === false) {
      this.logger.info(`Pulling ${this.utilityImage}`)
      await this.pullPromise(this.utilityImage)
      const image = await this.getImage(this.utilityImage)
      const managedImages = this.store.get('managedImages') as DockerState['managedImages']
      const newValue: string[] = [image.Id]
      if (managedImages !== undefined) {
        newValue.concat(managedImages)
      }
      await this.store.set('managedImages', newValue)
    }
  }

  public async makeSnapshot (services: StatefulServiceMap) {
    const snapshot = await this.store.initSnapshot()
    await this.ensureUtilityImageAvailable()
    for (const service of [...services.values()]) {
      this.logger.info(`Snapshotting service ${service.name}`)
      for (const volume of service.volumes) {
        await this.docker.run(
          this.utilityImage,
          ['sh', '-c', `cd ${service.dataPath} && tar cvf /snapshot/${volume}.tar .`],
          process.stdout,
          {
            HostConfig: {
              AutoRemove: true,
              Binds: [
                `${snapshot}:/snapshot`,
                `${volume}:${service.dataPath}`
              ]
            }
          }
        )
      }
    }
  }

  public async restoreSnapshot (snapshot: string, services: StatefulService[]) {
    await this.canRestore(snapshot)
    await this.ensureUtilityImageAvailable()
    for (const service of services) {
      this.logger.info(`Restoring service ${service.name}`)
      for (const volume of service.volumes) {
        const snapshotPath = path.join(this.store.snapshotsDir, snapshot, `${volume}.tar`)
        await this.docker.run(
          this.utilityImage,
          ['sh', '-c', 'cd /restore && tar xvf /snapshot.tar'],
          process.stdout,
          {
            HostConfig: {
              AutoRemove: true,
              Binds: [
                `${volume}:/restore`,
                `${snapshotPath}:/snapshot.tar`
              ]
            }
          }
        )
      }
    }
  }

  public async createVolume (name: string) {
    this.logger.info(`Creating volume ${name}...`)
    await this.docker.createVolume({
      Name: name
    })
  }

  public async removeVolume (name: string) {
    this.logger.info(`Removing volume ${name}...`)
    await this.docker.getVolume(name).remove()
  }

  public async cleanup (): Promise<void> {
    const managedImageIds = this.store.get('managedImages')
    if (managedImageIds === undefined) {
      this.logger.debug('No managed images to remove')
      return
    }
    for (const imageId of managedImageIds) {
      this.logger.info(`Removing managed image ${imageId}...`)
      const image = this.docker.getImage(imageId)
      this.logger.debug(image)
      await image.remove()
    }
  }

  private async getImage (name: string): Promise<Docker.ImageInfo> {
    const images = await this.docker.listImages()
    for (const image of images) {
      if (image.RepoTags && image.RepoTags.indexOf(name) !== -1) {
        return image
      }
    }
  }

  private async hasImage (name: string): Promise<boolean> {
    return await this.getImage(name) !== undefined
  }

  public async canRestore (snapshot: string) {
    return this.store.validateSnapshot(snapshot)
  }

  private pullPromise (imageName: string): Promise<void> {
    return new Promise((resolve, reject) => {
      this.docker.pull(imageName, {}, (error: Error, stream: Stream) => {
        if (error) return reject(error)
        const onFinished = (error: Error, output: any) => {
          if (error) return reject(error)
          resolve(output)
        }
        const onProgress = (event: ModemProgressEvent) => {
          if (event.progress) {
            this.logger.info(event.progress)
          }
        }
        this.docker.modem.followProgress(stream, onFinished, onProgress)
      })
    })
  }
}
