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
            const restResult = await getDataFromAPI("https://explorer.cardano.org/api/blocks/pages")
            const graphQLResult = await client.query({
                query: gql`query Blockheight {
                    cardano {
                        blockHeight
                    }    
                }`
            })

            let restResultBlockHeight = restResult["Right"][1][0]["cbeBlkHeight"]
            let graphQLBlockHeight = graphQLResult["data"]["cardano"]["blockHeight"]

            // As we're calling an external API to check equality on something that changes every 20 seconds
            // there is a small delta in the test condition to allow for this where the second API value can be
            // equal to or one less than the first API value. 

            expect(graphQLBlockHeight).toBeGreaterThan(restResultBlockHeight - 1)
            expect(graphQLBlockHeight).toBeLessThanOrEqual(restResultBlockHeight + 1);
        })
    })
}
