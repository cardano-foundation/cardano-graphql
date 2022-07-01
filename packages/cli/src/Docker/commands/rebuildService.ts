import { createCommand } from 'commander'
import inquirer from 'inquirer'
import { DockerComposeStack } from '../index'
import { selectServicesFromStack, servicesFromNameArg } from '../util'

export function rebuildServiceCommand (stack: DockerComposeStack) {
  return createCommand('rebuild-service')
    .description('Drop and rebuild data volumes for the specified service')
    .option('--services services', 'Service name', (names) => servicesFromNameArg(stack, names))
    .option('--no-backup', 'Rebuild without making a backup')
    .action(async (cmd) => {
      const { backup, services } = cmd
      if (services?.length > 0) {
        await stack.rebuildService(services, backup)
      } else {
        await inquirer
          .prompt([
            selectServicesFromStack(stack),
            {
              name: 'makeBackup',
              default: true,
              message: 'Make a backup first?',
              type: 'confirm'
            }
          ])
          .then(async (answers) => {
            const { makeBackup, services } = answers
            await stack.rebuildService(services, makeBackup)
          })
      }
      process.exit(0)
    })
}
