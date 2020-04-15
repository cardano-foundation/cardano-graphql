import gql from 'graphql-tag'
import { TestClient } from '../TestClient'
import fetch from 'node-fetch'

async function getDataFromAPI(url: string ) {
    let response = await fetch(url)
    let data = await response.json()
    return data
}

export function transactionTests(createClient: () => Promise<TestClient>) {

    describe('blocks', () => {
        let client: TestClient

        beforeEach(async () => {
            client = await createClient()
        }, 60000)

        it('returns the same height', async () => {
            const restResult = await getDataFromAPI("https://explorer.awstest.iohkdev.io/api-new/blocks/pages")
            const graphQLResult = await client.query({
                query: gql`query Blockheight {
                    cardano {
                        blockHeight
                    }    
                }`
            })

            expect(restResult["Right"][1][0]["cbeBlkHeight"]).toBe(graphQLResult["data"]["cardano"]["blockHeight"])
        })
    })
}
