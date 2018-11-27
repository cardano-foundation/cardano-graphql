import path from 'path'
import { upAll, down } from 'docker-compose'
import Docker from 'dockerode'
import on from 'wait-on'
import { expect } from 'chai'
import Api from './api'

const docker = new Docker()
const composeConfig = { cwd: path.join(__dirname), log: true }
const uri = 'http://localhost:4000/graphql'
const healthChecks = [
  'http://localhost:4000/.well-known/apollo/server-health'
]

suite('Integration with the REST API as an adaptor', function () {
  this.timeout(240000)

  suiteSetup(async function () {
    this.api = new Api (uri)
    await down(composeConfig)
    await upAll(composeConfig)
    await on({ resources: healthChecks })
    const wallets = await this.api.wallets()
    const sendingWallet = wallets[0]
    this.state = {
      sendingWallet,
      recipientAddresses: await this.api.addresses({ omit: sendingWallet.id})
    }
  })

  suiteTeardown(async function () {
    await down(composeConfig)
    const volume = docker.getVolume('cardano-graphql_wallet-state')
    await volume.remove({ force: true })
  })

  test('Queries', async function () {
    const nodeInfo = await this.api.nodeInfo()
    expect(nodeInfo.syncProgress).to.have.property('quantity').that.is.a('number')
  })

  test('Mutations', async function () {
    const transactionAmount = 12500
    const accounts = await this.api.walletAccounts({ walletId: this.state.sendingWallet.id })
    const transaction = await this.api.transfer({
      index: accounts[0].index,
      from: this.state.sendingWallet.id,
      to: this.state.recipientAddresses[0],
      amount: transactionAmount
    })
    expect(transaction).to.have.property('id').that.is.a('string')
    await transaction.persisted()
    const wallet = await this.api.walletById({ walletId: this.state.sendingWallet.id })
    const newBalance = wallet.balance
    expect(newBalance).to.equal(this.state.sendingWallet.balance - transactionAmount - transaction.fees)
  })
})
