/* eslint-disable quotes */

import fs from 'fs'
import { cleanTestData, createTransaction } from './transactionUtil'

function test () {
  const fromAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment.addr').toString().trim()
  const toAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment2.addr').toString().trim()
  const settings = {
    timeLimit: 300, // I can't find out how to work this out but it seems if you set it to 30 it's too low
    fromAddr,
    toAddr,
    signingKeyFile: '/app/payment.skey'
  }
  cleanTestData()
  const { txSignedFile, client } = createTransaction(settings)
  client.submitTransactionSync(txSignedFile)
  // wait for some time because it seems if you run the test again too soon it will fail, not sure how long though!
  // execSync(`sleep {settings.timeLimit}`)
}

describe('CLI', () => {
  it('submits a raw and signed transaction to the network using CLI', async () => {
    test()
  })
})
