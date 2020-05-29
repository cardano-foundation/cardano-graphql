#!/usr/bin/env node

import * as chalk from 'chalk'
import { Command } from 'commander'
import * as figlet from 'figlet'

import {
  init,
  backup,
  rebuildService
} from './commands'
import { DockerComposeStack } from './docker'
import * as inquirer from 'inquirer'
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
const bottomBar = new inquirer.ui.BottomBar()
const program = new Command('cgql')

const stack = new DockerComposeStack({ log: bottomBar.log.write })

program
  .addCommand(init)
  .addCommand(backup(stack))
  .addCommand(rebuildService(stack))

program.version(packageJson.version)

if (!process.argv.slice(2).length) {
  program.outputHelp()
  process.exit(1)
} else {
  program.parseAsync(process.argv)
}
