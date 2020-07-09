import fs from 'fs'
import path from 'path'
import { exec } from 'child_process'
import { DocumentNode } from 'graphql'
import { TestClient } from '@cardano-graphql/util-dev'
import util from '@cardano-graphql/util'
import { buildClient } from './util'
import axios from 'axios'

const mnemonics = [
  'fit', 'today', 'convince', 'mixture', 'crumble',
  'design', 'identify', 'swim', 'jelly', 'charge',
  'vast', 'expect', 'below', 'science', 'stove'
]

const cardanoWallet = 'docker run --rm -i inputoutput/cardano-wallet:2020.6.5-byron'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries'), name)
}

describe('submitTransaction', () => {
  let client: TestClient
  let signingKey: string

  beforeAll(async () => {
    client = await buildClient()
    return new Promise((resolve, reject) => {
      exec(`${cardanoWallet} key root --wallet-style icarus ${mnemonics.join(' ')} | ${cardanoWallet} key child --path 44H/1815H/0H/0/0`, (error, stdout, stderr) => {
        if (error && stderr) return reject(error)
        signingKey = stdout
        console.log(signingKey)
        resolve()
        exec(`${cardanoWallet} transaction fees 761ddaf7760aaf9c98cd8956417760c4b7f2c656 --payment 1@2cWKMJemoBajjWtQf8kVfCYMmaHbarDKLriPeMw3shDg1o2eQ19KfNKuU4nPvGYpUJppR`, (err, so, se) => {
          if (err && se) return reject(err)
          console.log(so)
          resolve()
        })
      })
    })
  })

  it('submits a raw and signed transaction to the network', async () => {
    const txPromise = new Promise((resolve, reject) => {
      exec(`cardano-tx empty 1097911063 | cardano-tx add-input 1 1d4ca4d72a1e02fbb79bec42392d9eb3da179f8a2316fd9e9a5ffe9c441d8bce | cardano-tx add-output 1 2cWKMJemoBajjWtQf8kVfCYMmaHbarDKLriPeMw3shDg1o2eQ19KfNKuU4nPvGYpUJppR | cardano-tx add-output 9831566 2cWKMJemoBajdXf1pQf2yUEsryKamaSDQ4RWW6Qq24yrufD3GJ2ioq3oEdmUce78f7qbu | cardano-tx lock | cardano-tx sign-with ${signingKey} | cardano-tx serialize --base16`, (error, stdout, stderr) => {
        if (error && stderr) return reject(error)
        console.log(typeof stdout)
        resolve(stdout)
      })
    })
    const result = await client.mutate({
      mutation: await loadQueryNode('submitTransaction'),
      variables: { tx: await txPromise }
    })
    console.log(result)
    expect(result).toBeDefined()
  })
  it('submits a raw and signed transaction to the network', async () => {
    const result = await client.mutate({
      mutation: await loadQueryNode('submitTransaction'),
      variables: { tx: '82839f8200d81858248258201d4ca4d72a1e02fbb79bec42392d9eb3da179f8a2316fd9e9a5ffe9c441d8bce01ff9f8282d818582883581c9c19ec69f7d3acad269e4bbbfcad67129c268bd772060b426b7e23cba102451a4170cb17001a1c281652018282d818582883581c95eaa323cac6cd8916a02c58b133d9b576ceb7bcb1f8c0716a701118a102451a4170cb17001a5e6fce581a0096048effa0818200d8185885825840b8cdd384ec1ef3dffe4db999d6bfce40afaa964543e2e1592c932f552fe9e8301233bbc1f45472837b904b719db5d32d947ec521e7fccd1aec6ad6cf884fc45858403964714f761d79a67b34c8e1fcc337af6ef44a894e6e740927dbec86d1be63e9987dc9ceb7905a53a0ddddf3a9ea4508e41a02d18ed3994461eb20cb0ba2710b%' }
    })
    console.log(result)
    expect(result).toBeDefined()
  })
  it('submits a raw and signed transaction to the network', async () => {
    const response = await axios('http://localhost:3100',{
      headers: 'Content-Type:application/cbor',
      data: fs.readFileSync('./tx1.tx')
    })
    console.log(response)
  })
})