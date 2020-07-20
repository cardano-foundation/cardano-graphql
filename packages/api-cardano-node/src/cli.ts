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
      if (error) {
        reject(error)
      } else if (stderr.toString() !== '') {
        reject(new Error(stderr.toString()))
      } else {
        resolve(JSON.parse(stdout))
      }
    })
  })
}

export function submitTransactionSync (txBodyFile: String) {
  execSync(`${cardanoCli} shelley transaction submit --tx-file ${txBodyFile} --testnet-magic 42`).toString()
}

export function submitTransaction (txBodyFile: String): Promise<String> {
  return new Promise((resolve, reject) => {
    exec(`${cardanoCli} shelley transaction submit --tx-file ${txBodyFile} --testnet-magic 42`, (error, stdout, stderr) => {
      if (error) {
        reject(error)
      } else if (stderr.toString() !== '') {
        reject(new Error(stderr.toString()))
      } else {
        resolve(stdout)
      }
    })
  })
}
