#!/usr/bin/env node

import * as chalk from 'chalk'
import { program } from 'commander'
import * as figlet from 'figlet'
import { createWriteStream, ensureDir, writeFile } from 'fs-extra'
import * as inquirer from 'inquirer'
import fetch from 'cross-fetch'
import * as generatePassword from 'password-generator'
import * as path from 'path'
const clear = require('clear')
const packageJson = require('../package.json')

clear()

console.log(
  chalk.blue(
    figlet.textSync(
      'cgql',
      {
        horizontalLayout: 'full',
        font: 'Stop'
      })
  )
)

program
  .name('cgql')
  .version(packageJson.version)

program
  .command('create-secrets')
  .description('Create secrets used for PostgreSQL. You will be prompted')
  .action(() =>
    inquirer
      .prompt([{
        name: 'db',
        default: 'cardano',
        message: 'A name for the database',
        type: 'input',
        validate: ensureValue
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
      .then(async ({ db, password, secretsDirPath, user }) => {
        await ensureDir(secretsDirPath)
        await Promise.all([
          writeFile(path.join(secretsDirPath, 'postgres_db'), db),
          writeFile(path.join(secretsDirPath, 'postgres_password'), password),
          writeFile(path.join(secretsDirPath, 'postgres_user'), user)
        ])
        console.log('PostgreSQL credentials created')
      })
  )

program
  .command('init-docker-compose')
  .description('Access the latest version of docker-compose to use as boilerplate')
  .option('-o, --out-dir <out>', 'Directory to write file', process.cwd())
  .action(async ({ outDir }) => {
    await ensureDir(outDir)
    const dockerComposeFile = await createWriteStream(path.join(outDir, 'docker-compose.yml'))
    const response = await fetch(
      'https://raw.githubusercontent.com/input-output-hk/cardano-graphql/master/docker-compose.yml'
    )
    await dockerComposeFile.write(await response.text())
    console.log(
      `A docker-compose file has been written into ${outDir}.
          While https://github.com/input-output-hk/cardano-graphql/issues/120 is open, you need to edit 
          HASURA_GRAPHQL_DATABASE_URL to match the newly created credentials, then run:
          NETWORK=testnet or mainnet docker-compose up -d
          `
    )
  })

if (!process.argv.slice(2).length) {
  program.outputHelp()
} else {
  program.parseAsync(process.argv)
}

function ensureValue (input: string) {
  return !input ? 'Cannot be blank' : true
}
