import * as fs from 'fs-extra'
import * as path from 'path'
import * as dockerCompose from 'docker-compose'
import * as Docker from 'dockerode'
import { cardanoNode, postgres, StatefulService } from './'

type logFunc = (message: string) => void

export class DockerComposeStack {
  readonly docker: Docker

  readonly log: logFunc

  readonly backupDir: string

  readonly projectName: string

  public statefulServices: Map<string, StatefulService>

  constructor ({
    log = console.log as logFunc,
    projectName = 'cardano-graphql',
    backupDir = path.join(process.cwd(), 'backups'),
    statefulServices = new Map([['postgres', postgres], ['cardano-node', cardanoNode]])
  }) {
    this.log = log
    this.statefulServices = statefulServices
    this.projectName = projectName
    this.backupDir = backupDir
    this.docker = new Docker()
  }

  // ************* LIFECYCLE ************************

  public up () {
    this.log('docker-compose up')
    return dockerCompose.upAll({
      composeOptions: ['-p', this.projectName]
    })
  }

  public down (commandOptions?: string[]) {
    this.log('docker-compose down')
    return dockerCompose.down({
      commandOptions
    })
  }

  public stop () {
    this.log('docker-compose stop')
    return dockerCompose.stop({
      composeOptions: ['-p', this.projectName]
    })
  }

  // *************************************************

  public async makeBackup (statefulServices: StatefulService[]): Promise<void> {
    await this.stop()
    for (const service of statefulServices) {
      const hostBackupDir = this.serviceBackupDir(service)
      await fs.ensureDir(hostBackupDir)
      this.log(`Backing up service ${service.name}`)
      await this.docker.run(
        'ubuntu',
        ['bash', '-c', `cd ${service.dataPath} && tar cvf /backup/${service.volumeName}_${Date.now()}.tar .`],
        process.stdout,
        {
          HostConfig: {
            AutoRemove: true,
            Binds: [
              `${hostBackupDir}:/backup`,
              `${this.volumeName(service)}:${service.dataPath}`
            ]
          }
        }
      )
    }
    await this.up()
  }

  public async restoreBackup (service: StatefulService, backupFile: string): Promise<void> {
    await this.validateBackup(service, backupFile)
    await this.down()
    await this.removeServiceVolumes([service])
    await this.createServiceVolumes([service])
    await this.docker.run(
      'ubuntu',
      ['bash', '-c', `cd /restore && tar xvf /backup.tar`],
      process.stdout,
      {
        HostConfig: {
          AutoRemove: true,
          Binds: [
            `${this.volumeName(service)}:/restore`,
            `${path.join(this.serviceBackupDir(service), backupFile)}:/backup.tar`
          ]
        }
      }
    )
    await this.up()
  }

  public async rebuildService (services: StatefulService[], backup = true): Promise<void> {
    try {
      if (backup === true) {
        await this.makeBackup(services)
      }
      await this.down()
      await this.removeServiceVolumes(services)
      await this.up()
    } catch (error) {
      console.error(error)
    }
  }

  private async createServiceVolumes (services: StatefulService[]): Promise<void> {
    for (const service of services) {
      this.log(`Creating volume ${service.volumeName}...`)
      await this.docker.createVolume({
        Name: this.volumeName(service)
      })
    }
  }

  private async removeServiceVolumes (services: StatefulService[]): Promise<void> {
    for (const service of services) {
      this.log(`Removing volume ${service.volumeName}...`)
      await this.docker.getVolume(this.volumeName(service)).remove()
    }
  }

  public async validateBackup (service: StatefulService, backupFile: string): Promise<void> {
    const fileStat = await fs.stat(path.join(this.serviceBackupDir(service), backupFile))
    if (!fileStat.isFile()) throw new Error('Invalid backup')
  }

  private serviceBackupDir (service: StatefulService) {
    return path.join(this.backupDir, service.name)
  }

  private volumeName (service: StatefulService) {
    return `${this.projectName}_${service.volumeName}`
  }
}
