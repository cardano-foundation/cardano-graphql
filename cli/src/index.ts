#!/usr/bin/env node

import chalk from 'chalk'
import figlet from 'figlet'
import { Command } from 'commander'
import { dockerCommand } from './Docker'
import { CliLogger } from './CliLogger'
import { systemInfoCommand } from './System'

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

const logger = new CliLogger()

const program = new Command('cgql')

program
  .addCommand(dockerCommand(logger))
  .addCommand(systemInfoCommand(packageJson.version))

program.version(packageJson.version)
if (!process.argv.slice(2).length) {
  program.outputHelp()
  process.exit(1)
} else {
  program.parseAsync(process.argv).catch(error => {
    console.error(error)
    process.exit(0)
  })
}
