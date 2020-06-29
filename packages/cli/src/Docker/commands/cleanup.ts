import { createCommand } from 'commander'
import { Logger } from 'ts-log'
import inquirer from 'inquirer'
import { DockerStore } from '../DockerStore'
import { DockerClient } from '../DockerClient'

export function cleanupCommand (store: DockerStore, dockerClient: DockerClient, logger: Logger) {
  return createCommand('cleanup')
    .description('Return the host to a clean state')
    .option('-f, --force', 'Skip prompt')
    .action(async (cmd) => {
      const { force } = cmd
      if (force === true) {
        await store.cleanup()
      } else {
        await inquirer
          .prompt([
            {
              name: 'proceed',
              default: false,
              message: 'This action will remove all stored config, managed Docker images, and snapshots. Proceed?',
              type: 'confirm'
            }
          ])
          .then(async (answers) => {
            const { proceed } = answers
            if (proceed === true) {
              return Promise.all([
                dockerClient.cleanup(),
                store.cleanup()
              ]).then(() => {
                logger.info('Cleanup complete')
              })
            } else {
              logger.info('Aborting cleanup')
            }
          })
      }
      process.exit(0)
    })
}
