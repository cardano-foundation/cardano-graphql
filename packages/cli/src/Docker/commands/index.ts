import { createCommand } from 'commander'
import { Logger } from 'ts-log'
import { DockerComposeStack } from '../DockerComposeStack'
import { cleanupCommand } from './cleanup'
import { initCommand } from './init'
import { snapshotCommand } from './snapshot'
import { rebuildServiceCommand } from './rebuildService'
import { DockerClient } from '../DockerClient'
import { DockerStore } from '../DockerStore'

export function dockerCommand (logger: Logger) {
  const store = new DockerStore(logger)
  const dockerClient = new DockerClient(store, { logger })

  const stack = new DockerComposeStack(
    store,
    dockerClient,
    { logger }
  )
  return createCommand('docker')
    .addCommand(initCommand(store, stack))
    .addCommand(snapshotCommand(store, stack))
    .addCommand(rebuildServiceCommand(stack))
    .addCommand(cleanupCommand(store, dockerClient, logger))
}
