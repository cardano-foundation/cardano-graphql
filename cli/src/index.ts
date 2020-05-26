#!/usr/bin/env node

import * as chalk from 'chalk'
import { Command } from 'commander'
import * as figlet from 'figlet'

import { init } from './commands'
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

const program = new Command('cgql')

program.version(packageJson.version)
program
  .addCommand(init)

if (!process.argv.slice(2).length) {
  program.outputHelp()
} else {
  program.parseAsync(process.argv)
}
