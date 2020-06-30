import inquirer from 'inquirer'
import path from 'path'
import { DockerStore } from '../DockerStore'
import { ensureValue } from '../../util'
import generatePassword from 'password-generator'
import { createCommand } from 'commander'
import { DockerComposeStack } from '../index'

const defaultCwd = process.cwd()

export function initCommand (store: DockerStore, stack: DockerComposeStack) {
  return createCommand('init')
    .description('Initialize a Docker stack with secrets using a boilerplate compose file.')
    .option('--cwd [cwd]', 'Set working directory', defaultCwd)
    .option('-p, --password [password]', 'Password for the database')
    .option('-u, --user [user]', 'User for the database')
    .action(async (cmd) => {
      const { cwd, password, user } = cmd
      if (password && user) {
        await stack.init(path.resolve(cwd))
        await store.createSecrets(password, user)
      } else {
        await inquirer
          .prompt([{
            name: 'directory',
            default: defaultCwd,
            message: 'Select a directory',
            type: 'input'
          }, {
            name: 'user',
            default: 'postgres',
            message: 'User for the database',
            type: 'input',
            validate: ensureValue
          }, {
            name: 'password',
            default: () => generatePassword(16, false),
            message: 'Password for the database user',
            type: 'password',
            mask: '*',
            validate: ensureValue
          }
          ])
          .then(async ({ directory, password, user }) => {
            await stack.init(directory)
            return store.createSecrets(password, user)
          })
      }
      process.exit(0)
    })
}
