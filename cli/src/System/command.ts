import { createCommand } from 'commander'
import { getSystemInfo } from './SystemInfo'

export function systemInfoCommand (version: string) {
  return createCommand('system-info')
    .description('Return useful information about the current system')
    .action(() => {
      console.log(getSystemInfo(version))
      process.exit(0)
    })
}
