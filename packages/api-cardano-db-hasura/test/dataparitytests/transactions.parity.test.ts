import gql from 'graphql-tag'
import utilDev, { TestClient } from '@cardano-graphql/util-dev'
import { buildSchema } from '@src/index'
import { getDataFromAPI } from './getDataFromApi'

describe('transactions', () => {
  let client: TestClient
  let restData: any
  let graphQLData: any

  beforeAll(async () => {
    if (process.env.TEST_MODE === 'e2e') {
      client = await utilDev.createE2EClient()
    } else {
      const schema = await buildSchema('http://localhost:8090')
      client = await utilDev.createIntegrationClient(schema)
    }
    const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
    restData = restResult.Right
    const graphQLResult = await client.query({
      query: gql`query TxByHash{
                        transactions (where: {hash: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                                                     {
                              hash
                              fee
                              block{
                                number
                                forgedAt
                              }
                              inputs(order_by: { sourceTxHash: asc }) {
                                address
                                value
                                sourceTxHash
                                sourceTxIndex
                              }
                              outputs(order_by: { index: asc }) {
                                address
                                index
                                value
                              }

                              totalOutput
                              includedAt
                            }
                          }`
    })
    graphQLData = graphQLResult.data.transactions[0]
  }, 30000)

  it('return the correct hash', async () => {
    const restResultId = restData.ctsId
    const graphQLHash = graphQLData.hash

    expect(graphQLHash).toEqual(restResultId)
  })

  it('return the correct Result Fee', async () => {
    const restResultFee = restData.ctsFees.getCoin
    const graphQLFee = graphQLData.fee

    expect(graphQLFee).toEqual(parseInt(restResultFee))
  })

  it('return the correct Total Output', async () => {
    const restResultTotalOutput = restData.ctsTotalOutput.getCoin
    const graphQLTotalOutput = graphQLData.totalOutput

    expect(graphQLTotalOutput).toEqual(restResultTotalOutput)
  })

  it('return the correct Block Height', async () => {
    const restResultTotalOutput = restData.ctsBlockHeight
    const graphQLTotalOutput = graphQLData.block.number

    expect(graphQLTotalOutput).toEqual(restResultTotalOutput)
  })

  it('return the correct Input Addresses', async () => {
    const restResultInputs = restData.ctsInputs
    const restResultAddresses = []

    for (const input of restResultInputs) {
      restResultAddresses.push(input[0])
    }

    const graphQLInputs = graphQLData.inputs
    const graphQLAddresses = []

    for (const input of graphQLInputs) {
      graphQLAddresses.push(input.address)
    }

    expect(graphQLAddresses).toEqual(restResultAddresses)
  })

  it('return the correct Input Values', async () => {
    const restResultInputs = restData.ctsInputs
    let restResultValues = 0

    for (const input of restResultInputs) {
      restResultValues += Number(input[1].getCoin)
    }

    const graphQLInputs = graphQLData.inputs
    let graphQLValues = 0

    for (const input of graphQLInputs) {
      graphQLValues += Number(input.value)
    }

    expect(graphQLValues).toEqual(restResultValues)
  })

  it('return the correct Output Addresses', async () => {
    const restResultOutputs = restData.ctsOutputs
    const restResultAddresses = []

    for (const output of restResultOutputs) {
      restResultAddresses.push(output[0])
    }

    const graphQLOutputs = graphQLData.outputs
    const graphQLAddresses = []

    for (const output of graphQLOutputs) {
      graphQLAddresses.push(output.address)
    }

    expect(graphQLAddresses).toEqual(restResultAddresses)
  })

  it('return the correct Output Values', async () => {
    const restResultOutputs = restData.ctsOutputs
    let restResultValues = 0

    for (const output of restResultOutputs) {
      restResultValues += Number(output[1].getCoin)
    }

    const graphQLOutputs = graphQLData.outputs
    let graphQLValues = 0

    for (const output of graphQLOutputs) {
      graphQLValues += Number(output.value)
    }

    expect(graphQLValues).toEqual(restResultValues)
  })

  it('have the same block creation time', async () => {
    const restResultBlockTime = restData.ctsBlockTimeIssued
    const graphQLBlockTime = graphQLData.block.forgedAt

    expect(graphQLBlockTime).toEqual(utilDev.timestampToIsoStringWithoutTimezone(restResultBlockTime))
  })

  it('have the same transaction inclusion time', async () => {
    const restResultTransactionTime = restData.ctsTxTimeIssued
    const graphQLTransactionTime = graphQLData.includedAt

    expect(graphQLTransactionTime).toEqual(utilDev.timestampToIsoStringWithoutTimezone(restResultTransactionTime))
  })
})
