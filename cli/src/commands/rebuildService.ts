import { createCommand } from 'commander'
import * as inquirer from 'inquirer'
import { DockerComposeStack } from '../docker'
import { serviceChoices, servicesFromNames } from './util'

export function rebuildService (stack: DockerComposeStack) {
  return createCommand('rebuild-service')
    .description('Drop and rebuild data volumes for the specified service')
    .option('--services [services]', 'Service name', (names) => servicesFromNames(stack, names))
    .option('--no-backup', 'Rebuild without making a backup')
    .action(async (cmd) => {
      const { backup, service } = cmd
      if (backup === false) {
        await stack.rebuildService(service, backup)
        process.exit(0)
      } else {
        return inquirer
          .prompt([{
            name: 'services',
            message: 'Select services',
            choices: serviceChoices(stack),
            type: 'checkbox',
            validate (input) {
              if (input.length === 0) {
                throw new Error('At least one service must be selected')
              }
              return true
            }
          }, {
            name: 'makeBackup',
            default: true,
            message: 'Make a backup first?',
            type: 'confirm'
          }])
          .then(async (answers) => {
            const { makeBackup, services } = answers
            await stack.rebuildService(services, makeBackup)
          })
      }
    })
}
