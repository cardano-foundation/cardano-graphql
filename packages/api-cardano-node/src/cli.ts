import { exec, execSync } from 'child_process'

export interface Settings {
  testnet: string
  cardanoCli: string
}

export type Tip = {
  blockNo: number,
  headerHash: String,
  slotNo: number
}

export class Client {
  testnet: string
  cardanoCli: string

  constructor (settings: Settings) {
    this.testnet = settings.testnet
    this.cardanoCli = settings.cardanoCli
  }

  getTipSync (): Tip {
    const stdout = execSync(`${this.cardanoCli} shelley query tip ${this.testnet}`).toString()
    return JSON.parse(stdout)
  }

  getTip (): Promise<Tip> {
    return new Promise((resolve, reject) => {
      exec(`${this.cardanoCli} shelley query tip ${this.testnet}`, (error, stdout, stderr) => {
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

  submitTransactionSync (txBodyFile: String) {
    execSync(`${this.cardanoCli} shelley transaction submit --tx-file ${txBodyFile} ${this.testnet}`).toString()
    console.log('signed')
  }

  submitTransaction (txBodyFile: String): Promise<String> {
    return new Promise((resolve, reject) => {
      exec(`${this.cardanoCli} shelley transaction submit --tx-file ${txBodyFile} ${this.testnet}`, (error, stdout, stderr) => {
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
}
