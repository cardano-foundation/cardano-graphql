import { createCommand } from 'commander'
import { createWriteStream, ensureDir, writeFile } from 'fs-extra'
import * as inquirer from 'inquirer'
import * as generatePassword from 'password-generator'
import * as path from 'path'
import { ensureValue } from '../util'
import fetch from 'cross-fetch'

export const init = createCommand('init')
  .description('Initialize a Docker stack with secrets using a boilerplate compose file.')
  .action(async () => {
    const dockerComposeFile = await createWriteStream('docker-compose.yml')
    const response = await fetch(
      'https://raw.githubusercontent.com/input-output-hk/cardano-graphql/master/docker-compose.yml'
    )
    await dockerComposeFile.write(await response.text())
    console.log(`docker-compose.yml created`)
    return inquirer
      .prompt([{
        name: 'user',
        default: 'postgres',
        message: 'User for the database',
        type: 'input',
        validate: ensureValue
      }, {
        name: 'password',
        default: () => generatePassword(16, false),
        message: 'Password for the database user',
        type: 'input',
        validate: ensureValue
      }, {
        name: 'secretsDirPath',
        default: 'config/secrets/',
        message: 'Secrets will be written to this directory',
        type: 'input',
        validate: ensureValue
      }
      ])
      .then(async ({ password, secretsDirPath, user }) => {
        await ensureDir(secretsDirPath)
        await Promise.all([
          writeFile(path.join(secretsDirPath, 'postgres_db'), 'cexplorer'),
          writeFile(path.join(secretsDirPath, 'postgres_password'), password),
          writeFile(path.join(secretsDirPath, 'postgres_user'), user)
        ])
        console.log('PostgreSQL credentials created')
      })
  })
