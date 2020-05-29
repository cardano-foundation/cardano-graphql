import { createCommand } from 'commander'
import * as inquirer from 'inquirer'
import * as path from 'path'
import * as fs from 'fs-extra'
import { DockerComposeStack } from '../docker'
import { serviceChoices, serviceFromName, servicesFromNames } from './util'

export function backup (stack: DockerComposeStack) {
  const backupCmd = createCommand('backup')
    .description('Make and restore backups of the stateful services')

  backupCmd
    .command('make')
    .description('Backup a service. Omit options to be prompted')
    .option('--services [services]', 'Comma separated list of service names', (names) => servicesFromNames(stack, names))
    .action(async (cmd) => {
      if (cmd.services === undefined) {
        return inquirer
          .prompt([{
            name: 'services',
            message: 'Select services to backup',
            choices: serviceChoices(stack),
            type: 'checkbox',
            validate (input) {
              if (input.length === 0) {
                throw new Error('At least one service must be selected')
              }
              return true
            }
          }])
          .then(({ services }) => stack.makeBackup(services))
      } else {
        await stack.makeBackup(cmd.services)
        process.exit(0)
      }
    })

  backupCmd
    .command('restore')
    .description('Restore a backup. Omit options to be prompted')
    .option('--backup-file [backupFile]', 'Filename of the backup')
    .option('--service [service]', 'Service name', (name) => serviceFromName(stack, name))
    .action(async (cmd) => {
      const { backupFile, service } = cmd
      if (service === undefined && backupFile === undefined) {
        return inquirer
          .prompt([{
            name: 'service',
            message: 'Select service',
            choices: serviceChoices(stack),
            type: 'list',
            validate (input) {
              if (input.length === 0) {
                throw new Error('At least one service must be selected')
              }
              return true
            }
          }, {
            name: 'backupFile',
            message: 'Select backup file to restore',
            choices: ({ service }) => fs.readdir(path.join(stack.backupDir, service.name)),
            type: 'list',
            validate (input) {
              if (input.length === 0) {
                throw new Error('At least one backup must be selected')
              }
              return true
            }
          }])
          .then(({ service, backupFile }) => stack.restoreBackup(service, backupFile))
      } else if (service !== undefined && backupFile !== undefined) {
        await stack.restoreBackup(service, backupFile)
        process.exit(0)
      } else {
        cmd.help()
      }
    })
  return backupCmd
}
