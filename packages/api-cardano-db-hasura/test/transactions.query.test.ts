import BigNumber from 'bignumber.js'
import path from 'path'
import { gql } from 'apollo-boost'
import { DocumentNode } from 'graphql'
import util from '@cardano-graphql/util'
import { TestClient } from '@cardano-graphql/util-dev'
import { allFieldsPopulated, init } from './util'
import Logger from 'bunyan'
import { Client, QueryResult } from 'pg'

function loadQueryNode (name: string): Promise<DocumentNode> {
  return util.loadQueryNode(path.resolve(__dirname, '..', 'src', 'example_queries', 'transactions'), name)
}

describe('transactions', () => {
  let logger: Logger
  let client: TestClient
  let db: Client
  beforeAll(async () => {
    ({ client, db, logger } = await init('transactions'))
    await db.connect()
  })
  afterAll(async () => {
    await db.end()
  })
  const getTestData = async (sql: string) :Promise<QueryResult> => {
    const resp = await db.query(sql)
    if (resp.rows.length === 0) logger.error('Can not find suitable data in db')
    expect(resp.rows.length).toBeGreaterThan(0)
    return resp
  }

  it('Returns transactions by hashes', async () => {
    const dbResp = await getTestData('SELECT hash FROM tx ORDER BY RANDOM() LIMIT 2;')
    const txHash1 = dbResp.rows[0].hash.toString('hex')
    const txHash2 = dbResp.rows[1].hash.toString('hex')
    logger.info('Hashes - ', txHash1, txHash2)
    const result = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: { hashes: [txHash1, txHash2] }
    })
    expect(result.data.transactions.length).toEqual(2)
  })

  it('Returns transactions by hashes with scripts', async () => {
    const dbRespPlutusV1 = await getTestData('SELECT t.hash FROM script JOIN tx t on t.id = script.tx_id WHERE type=\'plutusV1\' ORDER BY RANDOM() LIMIT 1;')
    const plutusV1hash = dbRespPlutusV1.rows[0].hash.toString('hex')
    logger.info('Plutus V1 hash - ', plutusV1hash)
    const plutusResult = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: { hashes: [plutusV1hash] }
    })
    expect(plutusResult.data.transactions.length).toEqual(1)
    allFieldsPopulated(plutusResult.data.transactions[0], 'amount')

    const dbRespPlutusV2 = await getTestData('SELECT t.hash FROM script JOIN tx t on t.id = script.tx_id WHERE type=\'plutusV2\' ORDER BY RANDOM() LIMIT 1;')
    const plutusV2hash = dbRespPlutusV2.rows[0].hash.toString('hex')
    logger.info('Plutus V2 hash - ', plutusV2hash)
    const plutusResultV2 = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: { hashes: [plutusV2hash] }
    })
    expect(plutusResultV2.data.transactions.length).toEqual(1)
    allFieldsPopulated(plutusResultV2.data.transactions[0], 'amount')

    const dbRespTimelock = await getTestData('SELECT t.hash FROM script JOIN tx t on t.id = script.tx_id WHERE type=\'timelock\' ORDER BY RANDOM() LIMIT 1;')
    const timelockTxHash = dbRespTimelock.rows[0].hash.toString('hex')
    logger.info('Timelock tx hash - ', timelockTxHash)
    const timelockResult = await client.query({
      query: await loadQueryNode('transactionsByHashesOrderByFee'),
      variables: { hashes: [timelockTxHash] }
    })
    expect(timelockResult.data.transactions.length).toEqual(1)
    allFieldsPopulated(plutusResult.data.transactions[0], 'amount')
  })

  it('Can return ordered by block index', async () => {
    const dbRespTimelock = await getTestData('select * from block where tx_count > 0 ORDER BY RANDOM() LIMIT 1;')
    const blockNo = dbRespTimelock.rows[0].block_no
    logger.info('Block number - ', blockNo)
    const result = await client.query({
      query: await loadQueryNode('orderedTransactionsInBlock'),
      variables: { blockNumber: blockNo }
    })
    expect(result.data.transactions.length.toString()).toBe(dbRespTimelock.rows[0].tx_count)
    expect(result.data.transactions[0].hash).toBeDefined()
  })

  it('returns an empty array when the transactions has no outputs', async () => {
    const dbRespTimelock = await getTestData('select hash from tx left join tx_out t on tx.id = t.tx_id where t.tx_id is NULL ORDER BY RANDOM() LIMIT 1;')
    const txHash = dbRespTimelock.rows[0].hash.toString('hex')
    logger.info('Tx hash - ', txHash)
    const result = await client.query({
      query: gql`query transactionWithNoOutputs(
          $hash: Hash32Hex!
      ) {
          transactions(
              where: { hash: { _eq: $hash } },
          ) {
              outputs {
                  address
                  value
              }
              outputs_aggregate {
                  aggregate {
                      count
                  }
              }
              inputs_aggregate {
                  aggregate {
                      count
                  }
              }
              totalOutput
          }
      }`,
      variables: { hash: txHash }
    })
    expect(result.data.transactions.length).toBe(1)
    expect(result.data.transactions[0].outputs_aggregate.aggregate.count).toBe('0')
    expect(result.data.transactions[0].outputs).toEqual([])
    expect(result.data.transactions[0].totalOutput).toEqual('0')
    expect(result.data.transactions[0].inputs_aggregate.aggregate.count).toBe('1')
  })

  it('Returns correct tx data from db', async () => {
    const dbRespTimelock = await getTestData('SELECT hash, out_sum FROM tx JOIN (SELECT id FROM tx ORDER BY out_sum DESC LIMIT 100) AS t ON tx.id=t.id ORDER BY RANDOM() LIMIT 1;')
    const txHash = dbRespTimelock.rows[0].hash.toString('hex')
    logger.info('Tx hash - ', txHash)
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinTransaction'),
      variables: {
        hashes: [txHash]
      }
    })
    const { transactions: txs } = result.data
    expect(txs[0].outputs_aggregate.aggregate.sum.value).toEqual(dbRespTimelock.rows[0].out_sum)
  })
  it('Can return aggregated data', async () => {
    const dbRespTimelock = await getTestData('SELECT hash FROM tx LEFT JOIN tx_out t ON tx.id = t.tx_id WHERE t.tx_id IS NOT NULL AND tx.deposit>0 ORDER BY RANDOM() LIMIT 1;')
    const txHash = dbRespTimelock.rows[0].hash.toString('hex')
    logger.info('Tx hash - ', txHash)
    const result = await client.query({
      query: await loadQueryNode('aggregateDataWithinTransaction'),
      variables: {
        hashes: [txHash]
      }
    })
    const { transactions: txs } = result.data
    const outputsPlusFee = new BigNumber(txs[0].outputs_aggregate.aggregate.sum.value).plus(txs[0].fee).plus(txs[0].deposit).toString()
    expect(txs[0].inputs_aggregate.aggregate.sum.value).toEqual(outputsPlusFee)
  })
  it('Can return filtered aggregated data', async () => {
    const dbRespTimelock = await getTestData('select hash, address from tx join tx_out t on tx.id = t.tx_id where index > 0 ORDER BY RANDOM() LIMIT 1;')
    const txHash = dbRespTimelock.rows[0].hash.toString('hex')
    logger.info('Tx hash - ', txHash)
    const addr = dbRespTimelock.rows[0].address
    const result = await client.query({
      query: await loadQueryNode('filteredAggregateDataWithinTransaction'),
      variables: {
        hash: txHash,
        outputsAddress: addr
      }
    })
    allFieldsPopulated(result.data.transactions)
  })

  describe('metadata', () => {
    it('JSON object', async () => {
      const dbRespTimelock = await getTestData('select hash from tx_metadata join tx t on t.id = tx_metadata.tx_id ORDER BY RANDOM() LIMIT 1;')
      const txHash = dbRespTimelock.rows[0].hash.toString('hex')
      logger.info('Tx hash - ', txHash)
      const result = await client.query({
        query: await loadQueryNode('transactionByIdWithMetadataIfPresent'),
        variables: { hash: txHash }
      })
      expect(result.data.transactions[0].metadata).toBeDefined()
    })
  })

  describe('transactions with tokens', () => {
    it('shows the tokens minted and output', async () => {
      const dbRespTimelock = await getTestData('select * from ma_tx_mint join tx t on ma_tx_mint.tx_id = t.id where quantity > 0 ORDER BY RANDOM() LIMIT 1;')
      const txHash = dbRespTimelock.rows[0].hash.toString('hex')
      logger.info('Tx hash - ', txHash)
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithTokens'),
        variables: { hashes: [txHash] }
      })
      expect(result.data.transactions[0].mint[0].asset.assetId).toBeDefined()
      expect(result.data.transactions[0].mint[0].asset.policyId).toBeDefined()
      expect(result.data.transactions[0].outputs[0].address).toBeDefined()
    })
  })

  describe('transactions with collateral', () => {
    it('shows the collateral inputs and outputs', async () => {
      const dbRespTimelock = await getTestData('select hash from collateral_tx_out join tx t on collateral_tx_out.tx_id = t.id ORDER BY RANDOM() LIMIT 1;')
      const txHash = dbRespTimelock.rows[0].hash.toString('hex')
      logger.info('Tx hash - ', txHash)
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithCollateral'),
        variables: { hashes: [txHash] }
      })
      expect(result.data.transactions[0].collateralInputs.length).toBeGreaterThan(0)
      expect(result.data.transactions[0].collateralOutputs.length).toBeGreaterThan(0)
    })
  })

  describe('transactions with reference inputs', () => {
    it('shows the reference inputs', async () => {
      const dbRespTimelock = await getTestData('select hash from reference_tx_in join tx t on t.id = reference_tx_in.tx_in_id ORDER BY RANDOM() LIMIT 1;')
      const txHash = dbRespTimelock.rows[0].hash.toString('hex')
      logger.info('Tx hash - ', txHash)
      const result = await client.query({
        query: await loadQueryNode('transactionsByHashesWithReferenceInputs'),
        variables: { hashes: [txHash] }
      })
      expect(result.data.transactions[0].referenceInputs.length).toBeGreaterThan(0)
    })
  })
})
