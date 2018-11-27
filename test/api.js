import gql from 'graphql-tag'
import { createHttpLink } from 'apollo-link-http'
import delay from 'delay'
import fetch from 'node-fetch'
import { execute, makePromise } from 'apollo-link'

export default class Api {
  link = null;

  constructor (uri) {
    this.link = createHttpLink({ uri, fetch });
  }

  async nodeInfo() {
    const response = await this._request({
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
    return response.get_api_v1_node_info.data
  }

  async addresses (options) {
    const response = await this._request({
      query: gql`query {
        get_api_v1_addresses {
            data {
                id
            }
        }}`
    })
    const { data } = response.get_api_v1_addresses
    if (options.omit) {
      const sendingAccountAddresses = await this.accountAddresses({ walletId: options.omit });
      return data.map(a => a.id).filter(a => !sendingAccountAddresses.includes(a))
    }
    return data
  }

  async getTransaction (variables) {
    const response = await this._request({
      query: gql`query GetTransaction($id: String!) {
          get_api_v1_transactions(id: $id) {
              data {
                  status {
                      tag
                  }
              }
          }}`,
      variables
    })
    return response.get_api_v1_transactions.data
  }

  async transactionPersisted (id) {
    const transactions = await this.getTransaction({ id })
    if (transactions[0].status.tag === 'persisted') return true
    // Keep checking until transaction is persisted in the blockchain
    await delay(2000)
    return this.transactionPersisted(id)
  }

  async accountAddresses (variables) {
    const response = await this._request({
      query: gql`query AccountAddressesQuery($walletId: String!) {
          get_api_v1_wallets_walletId_accounts(walletId: $walletId) {
              data { addresses { id } }
          }
      }`,
      variables
    })
    const { data } = response.get_api_v1_wallets_walletId_accounts
    return data.reduce((addresses, account) => [...addresses, ...account.addresses.map(a => a.id)], [])
  }

  async estimatedFees(variables) {
    const response = await this._request({
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
      variables
    })
    return response.post_api_v1_transactions_fees.data.estimatedAmount

  }

  async walletAccounts(variables) {
    const response = await this._request({
      query: gql`query GetWalletAccounts($walletId: String!) {
          get_api_v1_wallets_walletId_accounts(walletId: $walletId, page: 1, per_page: 50) {
              data {
                  index
              }
          }
      }`,
      variables
    })
    return response.get_api_v1_wallets_walletId_accounts.data
  }

  async walletById (variables) {
    const response = await this._request({
      query: gql`query GetWalletById($walletId: String!){
          get_api_v1_wallets_walletId(walletId: $walletId) {
              data {
                  id,
                  balance
              }
          }
      }`,
      variables
    })
    return response.get_api_v1_wallets_walletId.data
  }

  async wallets () {
    const response = await this._request({
      query: gql`query {
          get_api_v1_wallets {
              data {
                  id,
                  balance
              }
          }}`
    })
    const { get_api_v1_wallets } = response;
    if (!get_api_v1_wallets || get_api_v1_wallets.data.length < 1) {
      // Demo cluster may eventually have demo wallet data, so keep retrying
      await delay(1000)
      return this.wallets()
    }
    return get_api_v1_wallets.data;
  }

  async transfer(variables) {
    const estimatedFees = await this.estimatedFees(variables)
    const response = await this._request({
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
              data { id, amount }
          }
      }`,
      variables
    })
    const transaction = response.post_api_v1_transactions.data;
    transaction.persisted = () => this.transactionPersisted(transaction.id)
    transaction.fees = estimatedFees
    return transaction
  }

  async _request (operation) {
    try {
      const result = await makePromise(execute(this.link, operation))
      return result.data
    } catch (error) {
     console.error(error.message)
    }
  }
}
