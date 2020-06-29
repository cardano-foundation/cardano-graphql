import { createCommand } from 'commander'
import inquirer from 'inquirer'
import { DockerComposeStack } from '../DockerComposeStack'
import { DockerStore } from '../DockerStore'

export function snapshotCommand (store: DockerStore, stack: DockerComposeStack) {
  const snapshotCmd = createCommand('snapshot')
    .description('Make and restore snapshots of the stateful services')

  snapshotCmd
    .command('make')
    .description('Snapshot the stack')
    .action(async () => {
      await stack.makeSnapshot()
      process.exit(0)
    })

  snapshotCmd
    .command('restore')
    .description('Restore a snapshot. Omit options to be prompted')
    .option('-l, --last', 'Restore the most recent snapshot')
    .action(async (cmd) => {
      const { last } = cmd
      if (last === true) {
        await stack.restoreSnapshot()
      } else {
        await inquirer
          .prompt([
            {
              name: 'snapshot',
              message: 'Select snapshot to restore',
              choices: async () => store.snapshotsList(),
              type: 'list',
              validate (input) {
                if (input.length === 0) {
                  throw new Error('At least one snapshot must be selected')
                }
                return true
              }
            }
          ])
          .then(({ snapshot }) => stack.restoreSnapshot(snapshot))
      }
      process.exit(0)
    })

  return snapshotCmd
}
