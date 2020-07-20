import { exec, execSync } from 'child_process'

export type Tip = {
  blockNo: number,
  headerHash: String,
  slotNo: number
}

const cardanoCli = process.env.CARDANO_CLI_CMD ? process.env.CARDANO_CLI_CMD : 'cardano-cli'

export function getTipSync (): Tip {
  const stdout = execSync(`${cardanoCli} shelley query tip --testnet-magic 42`).toString()
  return JSON.parse(stdout)
}

export function getTip (): Promise<Tip> {
  return new Promise((resolve, reject) => {
    exec(`${cardanoCli} shelley query tip --testnet-magic 42`, (error, stdout, stderr) => {
      console.log(error, stdout, stderr)
      if (error) {
        console.log('error: ', error)
        reject(error)
      } else if (stderr.toString() !== '') {
        console.log('stderr: ', stderr.toString())
        reject(new Error(stderr.toString()))
      } else {
        console.log('stdout: ', stdout)
        resolve(JSON.parse(stdout))
      }
    })
  })
}
