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

            const restResultFee = restResult['Right']['ctsFees']['getCoin']
            const graphQLFee = graphQLResult['data']['transactions'][0]['fee']

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

        
    })
}
