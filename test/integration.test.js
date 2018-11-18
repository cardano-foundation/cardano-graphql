import path from 'path'
import { upAll, down } from 'docker-compose'
import Docker from 'dockerode'
import delay from 'delay'
import gql from 'graphql-tag'
import on from 'wait-on'
import fetch from 'node-fetch'
import { execute, makePromise } from 'apollo-link'
import { createHttpLink } from 'apollo-link-http'
import { expect } from 'chai'

const docker = new Docker()
const composeConfig = { cwd: path.join(__dirname), log: true }
const uri = 'http://localhost:4000/graphql'
const healthChecks = [
  'http://localhost:4000/.well-known/apollo/server-health'
]
const link = createHttpLink({ uri, fetch })

const executeOperation = (operation) => makePromise(execute(link, operation))

async function allAddresses () {
  const response = await executeOperation({
    query: gql`query {
        get_api_v1_addresses {
            data {
                id
            }
        }}`
  })
  return response.data.get_api_v1_addresses.data.map(a => a.id)
}

async function accountAddresses (walletId) {
  const response = await executeOperation({
    query: gql`query AccountAddressesQuery($walletId: String!) {
        get_api_v1_wallets_walletId_accounts(walletId: $walletId) {
            data { addresses { id } }
        }
    }`,
    variables: {
      walletId
    }
  })
  return response.data.get_api_v1_wallets_walletId_accounts.data
    .reduce((addresses, account) => [...addresses, ...account.addresses.map(a => a.id)], [])
}

async function fetchDemoData () {
  const walletResponse = await executeOperation({
    query: gql`query {
        get_api_v1_wallets {
            data {
                id,
                balance
            }
        }}`
  })
  const { data: { get_api_v1_wallets: { data: wallets } } } = walletResponse
  if (wallets.length === 0) {
    // Demo cluster will eventually have demo wallet data, so keep retrying
    await delay(1000)
    return fetchDemoData()
  }
  const sendingWallet = wallets[0]
  const addresses = await allAddresses()
  const sendingAccountAddresses = await accountAddresses(sendingWallet.id)
  const recipientAddresses = addresses.filter(a => !sendingAccountAddresses.includes(a))
  return { sendingWallet, recipientAddresses }
}

async function getTransaction (id) {
  const response = await executeOperation({
    query: gql`query GetTransaction($id: String!) {
        get_api_v1_transactions(id: $id) {
            data {
                status {
                    tag
                }
            }
        }}`,
    variables: {
      id
    }
  })
  return response.data.get_api_v1_transactions.data
}

async function transactionPersisted (id) {
  const transaction = await getTransaction(id)
  if (transaction[0].status.tag === 'persisted') return true
  // Keep checking until transaction is persisted in the blockchain
  await delay(2000)
  return transactionPersisted(id)
}

suite('Integration with the REST API as an adaptor', function () {
  this.timeout(240000)

  suiteSetup(async function () {
    await down(composeConfig)
    await upAll(composeConfig)
    await on({ resources: healthChecks })
    this.demoData = await fetchDemoData()
  })

  suiteTeardown(async function () {
    await down(composeConfig)
    const volume = docker.getVolume('cardano-graphql_wallet-state')
    await volume.remove({ force: true })
  })

  test('Queries', async function () {
    const result = await executeOperation({
      query: gql`query {
          get_api_v1_node_info {
              data {
                  syncProgress {
                      quantity
                        unit
                  }
              }
          }
      }`
    })
    expect(result.data).to.have.property('get_api_v1_node_info')
    expect(result.data.get_api_v1_node_info.data.syncProgress)
      .to.have.property('quantity').that.is.a('number')
  })

  test('Mutations', async function () {
    const transactionAmount = 1250000
    const accountResponse = await executeOperation({
      query: gql`query GetWalletAccounts($walletId: String!) {
          get_api_v1_wallets_walletId_accounts(walletId: $walletId, page: 1, per_page: 50) {
              data {
                  index
              }
          }
      }`,
      variables: {
        walletId: this.demoData.sendingWallet.id
      }
    })
    const { data: { get_api_v1_wallets_walletId_accounts: { data: accounts } } } = accountResponse

    const estimatedFeesResponse = await executeOperation({
      query: gql`mutation FeeEstimation($index: Float!, $from: String!, $to: String!, $amount: Float!) {
          post_api_v1_transactions_fees(body: {
              source: {
                  accountIndex: $index,
                  walletId: $from
              },
              destinations: [{
                  address: $to,
                  amount: $amount
              }]
          }) {
              data { estimatedAmount }
          }
      }`,
      variables: {
        index: accounts[0].index,
        from: this.demoData.sendingWallet.id,
        to: this.demoData.recipientAddresses[0],
        amount: transactionAmount
      }
    })
    const { estimatedAmount: fees } = estimatedFeesResponse.data.post_api_v1_transactions_fees.data
    const transferResult = await executeOperation({
      query: gql`mutation TransferMutation($index: Float!, $from: String!, $to: String!, $amount: Float!) {
          post_api_v1_transactions(body: {
              source: {
                  accountIndex: $index,
                  walletId: $from
              },
              destinations: [{
                  address: $to,
                  amount: $amount
              }]
          }) {
              data { id }
          }
      }`,
      variables: {
        index: accounts[0].index,
        from: this.demoData.sendingWallet.id,
        to: this.demoData.recipientAddresses[0],
        amount: transactionAmount
      }
    })
    const { data } = transferResult
    expect(data).to.have.property('post_api_v1_transactions')
    expect(data.post_api_v1_transactions.data)
      .to.have.property('id').that.is.a('string')
    const { id } = data.post_api_v1_transactions.data
    await transactionPersisted(id)
    const walletResponse = await executeOperation({
      query: gql`query GetWalletById($walletId: String!){
          get_api_v1_wallets_walletId(walletId: $walletId) {
              data {
                  id,
                  balance
              }
          }
      }`,
      variables: {
        walletId: this.demoData.sendingWallet.id
      }
    })
    const { data: { get_api_v1_wallets_walletId: { data: wallet } } } = walletResponse
    const newBalance = wallet.balance
    expect(newBalance).to.equal(this.demoData.sendingWallet.balance - transactionAmount - fees)
  })
})
