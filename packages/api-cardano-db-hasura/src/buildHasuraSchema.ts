import { fetch } from 'cross-fetch'
import { introspectSchema, wrapSchema } from '@graphql-tools/wrap'
import { DocumentNode, print } from 'graphql'

export async function buildHasuraSchema (hasuraUri: string) {
  const executor = async ({ document, variables }: { document: DocumentNode, variables?: Object }) => {
    const query = print(document)
    try {
      const fetchResult = await fetch(`${hasuraUri}/v1/graphql`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-Hasura-Role': 'cardano-graphql'
        },
        body: JSON.stringify({ query, variables })
      })
      return fetchResult.json()
    } catch (error) {
      console.error(error)
      throw error
    }
  }

  return wrapSchema({
    schema: await introspectSchema(executor),
    executor
  })
}
