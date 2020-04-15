import gql from 'graphql-tag'
import { TestClient } from '../TestClient'

export function transactionTests(createClient: () => Promise<TestClient>) {
    describe('blocks', () => {
        let client: TestClient

        beforeEach(async () => {
            client = await createClient()
        }, 60000)

        it('caps the response to 100 blocks', async () => {
            const result = await client.query({
                query: gql`query {
                    blocks {
                    id
                    }
                }`
            })
            expect(result.data.blocks.length).toBe(100)
        })
    })
}
