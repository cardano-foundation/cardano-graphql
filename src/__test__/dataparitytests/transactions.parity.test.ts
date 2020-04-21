import gql from 'graphql-tag'
import { TestClient } from '../TestClient'
import fetch from 'node-fetch'

async function getDataFromAPI(path: string) {
    const response = await fetch(`https://explorer.cardano.org/api/${path}`)
    return response.json()
}

export function transactionTests(createClient: () => Promise<TestClient>) {
    describe('transactions', () => {
        let client: TestClient

        beforeEach(async () => {
            client = await createClient()
        }, 60000)

        it('return the correct ID', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                          transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                          {
                            id
                          }
                        }`
                })

            const restResultId = restResult['Right']['ctsId']
            const graphQLId = graphQLResult['data']['transactions'][0]['id']

            expect(graphQLId).toEqual(restResultId)
        })

        it('return the correct Result Fee', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                          transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                          {
                            fee
                          }
                        }`
            })

            const restResultFee = Number(restResult['Right']['ctsFees']['getCoin'])
            const graphQLFee = Number(graphQLResult['data']['transactions'][0]['fee'])

            expect(graphQLFee).toEqual(restResultFee)
        })

        it('return the correct Total Output', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                          transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                          {
                            totalOutput
                          }
                        }`
            })

            const restResultTotalOutput = restResult['Right']['ctsTotalOutput']['getCoin']
            const graphQLTotalOutput = graphQLResult['data']['transactions'][0]['totalOutput']

            expect(graphQLTotalOutput).toEqual(restResultTotalOutput)
        })

        it('return the correct Block Height', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                              transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                              {
                                id
                                fee
                                block{
                                  number
                                  createdAt
                                }
                                inputs {
                                  address
                                  value
                                  sourceTxId
                                  sourceTxIndex
                                }
                                outputs{
                                  address
                                  index
                                  value
                                }
    
                                totalOutput
                                includedAt
                              }
                            }`
            })

            const restResultTotalOutput = restResult['Right']['ctsBlockHeight']
            const graphQLTotalOutput = graphQLResult['data']['transactions'][0]['block']['number']

            expect(graphQLTotalOutput).toEqual(restResultTotalOutput)
        })

        it('return the correct Input Addresses', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                              transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                              {
                                id
                                fee
                                block{
                                  number
                                  createdAt
                                }
                                inputs {
                                  address
                                  value
                                  sourceTxId
                                  sourceTxIndex
                                }
                                outputs{
                                  address
                                  index
                                  value
                                }
    
                                totalOutput
                                includedAt
                              }
                            }`
            })

            let restResultInputs = restResult['Right']['ctsInputs']
            let restResultAddresses = []

            for (const input of restResultInputs) {
                restResultAddresses.push(input[0])
            }

            let graphQLInputs = graphQLResult['data']['transactions'][0]['inputs']
            let graphQLAddresses = []

            for (const input of graphQLInputs) {
                graphQLAddresses.push(input['address'])
            }

            expect(graphQLAddresses.sort()).toEqual(restResultAddresses.sort())
        })

        it('return the correct Input Values', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                              transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                              {
                                id
                                fee
                                block{
                                  number
                                  createdAt
                                }
                                inputs {
                                  address
                                  value
                                  sourceTxId
                                  sourceTxIndex
                                }
                                outputs{
                                  address
                                  index
                                  value
                                }
    
                                totalOutput
                                includedAt
                              }
                            }`
            })

            let restResultInputs = restResult['Right']['ctsInputs']
            let restResultValues = 0

            for (const input of restResultInputs) {
                restResultValues += Number(input[1]['getCoin'])
            }

            let graphQLInputs = graphQLResult['data']['transactions'][0]['inputs']
            let graphQLValues = 0

            for (const input of graphQLInputs) {
                graphQLValues += Number(input['value'])
            }

            expect(graphQLValues).toEqual(restResultValues)
        })

        it('return the correct Output Addresses', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                              transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                              {
                                id
                                fee
                                block{
                                  number
                                  createdAt
                                }
                                inputs {
                                  address
                                  value
                                  sourceTxId
                                  sourceTxIndex
                                }
                                outputs{
                                  address
                                  index
                                  value
                                }

                                totalOutput
                                includedAt
                              }
                            }`
            })

            let restResultOutputs = restResult['Right']['ctsOutputs']
            let restResultAddresses = []

            for (const output of restResultOutputs) {
                restResultAddresses.push(output[0])
            }

            let graphQLOutputs = graphQLResult['data']['transactions'][0]['outputs']
            let graphQLAddresses = []

            for (const output of graphQLOutputs) {
                graphQLAddresses.push(output['address'])
            }

            expect(graphQLAddresses.sort()).toEqual(restResultAddresses.sort())
        })

        it('return the correct Output Values', async () => {
            const restResult = await getDataFromAPI('txs/summary/1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762')
            const graphQLResult = await client.query({
                query: gql`query TxById{
                              transactions (where: {id: {_eq:"1ac36644733c367ee4c551413d799d2e395d6ddfe14bebf1c281e6e826901762"}})
                              {
                                id
                                fee
                                block{
                                  number
                                  createdAt
                                }
                                inputs {
                                  address
                                  value
                                  sourceTxId
                                  sourceTxIndex
                                }
                                outputs{
                                  address
                                  index
                                  value
                                }
    
                                totalOutput
                                includedAt
                              }
                            }`
            })

            let restResultOutputs = restResult['Right']['ctsOutputs']
            let restResultValues = 0

            for (const output of restResultOutputs) {
                restResultValues += Number(output[1]['getCoin'])
            }

            let graphQLOutputs = graphQLResult['data']['transactions'][0]['outputs']
            let graphQLValues = 0

            for (const output of graphQLOutputs) {
                graphQLValues += Number(output['value'])
            }

            expect(graphQLValues).toEqual(restResultValues)
        })
    })
}
