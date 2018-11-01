import path from 'path'
import { upAll, down } from 'docker-compose';
import Docker from 'dockerode';
import delay from 'delay';
import gql from 'graphql-tag';
import on from 'wait-on';
import fetch from 'node-fetch';
import { execute, makePromise } from 'apollo-link';
import { createHttpLink } from 'apollo-link-http';
import { expect } from 'chai';

const docker = new Docker();
const composeConfig = { cwd: path.join(__dirname), log: true };
const uri = 'http://localhost:4000/graphql';
const healthChecks = [
  'http://localhost:4000/.well-known/apollo/server-health'
];

const executeOperation = (link, operation) => makePromise(execute(link, operation));

const fetchDemoData = async (link) => {
  const walletResponse = await executeOperation(link, {
    query: gql`query {
        get_api_v1_wallets {
            data {
                id
            }
        }}`
  });
  const { data: { get_api_v1_wallets: { data: wallets }}} = walletResponse;
  if (wallets.length === 0) {
    // Demo cluster will eventually have demo wallet data, so keep retrying
    await delay(1000);
    return fetchDemoData(link);
  }
  const addressResponse = await executeOperation(link, {
    query: gql`query {
        get_api_v1_addresses {
            data {
                id
            }
        }}`
  });
  const { data: { get_api_v1_addresses: { data: addresses }}} = addressResponse;
  return { wallets, addresses };
};

suite('Integration with the REST API as an adaptor', function () {

  this.timeout(240000);

  suiteSetup(async function () {
    await down(composeConfig);
    await upAll(composeConfig);
    await on({ resources: healthChecks });
    this.apolloLink = createHttpLink({
      uri,
      fetch
    });
    this.demoData = await fetchDemoData(this.apolloLink);
  });

  suiteTeardown(async function() {
    await down(composeConfig);
    const volume = docker.getVolume('cardano-graphql_wallet-state');
    await volume.remove({ force: true });
  });

  test('Queries', async function() {
    const result = await executeOperation(this.apolloLink, {
      query: gql`query GetNodeInfo {
          get_api_v1_node_info {
              data {
                  syncProgress {
                      quantity
                        unit
                  }
              }
          }
      }`
    });
    expect(result.data).to.have.property('get_api_v1_node_info');
    expect(result.data.get_api_v1_node_info.data.syncProgress)
      .to.have.property('quantity').that.is.a('number');
  });

  test('Mutations', async function() {
    const accountResponse = await executeOperation(this.apolloLink, {
      query: gql`query GetWalletAccounts($walletId: String!) {
          get_api_v1_wallets_walletId_accounts(walletId: $walletId, page: 1, per_page: 50) {
              data {
                  index
              }
          }
      }`,
      variables: {
        walletId: this.demoData.wallets[0].id
      }
    });
    const { data: { get_api_v1_wallets_walletId_accounts: { data: accounts }}} = accountResponse;
    const result = await executeOperation(this.apolloLink, {
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
        from: this.demoData.wallets[0].id,
        to: this.demoData.addresses[0].id,
        amount: 1
      }
    });
    expect(result.data).to.have.property('post_api_v1_transactions');
    expect(result.data.post_api_v1_transactions.data)
      .to.have.property('id').that.is.a('string');
  });

});
