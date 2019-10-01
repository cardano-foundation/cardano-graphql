import { RetryPromise } from 'promise-exponential-retry'
import { createHttpLink } from 'apollo-link-http'
import { fetch } from 'cross-fetch'
import { introspectSchema, makeRemoteExecutableSchema } from 'graphql-tools'

export async function buildHasuraSchema (hasuraUri: string) {
  const link = createHttpLink({
    uri: hasuraUri,
    fetch
  })

  await RetryPromise.retryPromise('Hasura Schema introspection', async () => {
    const schema = await introspectSchema(link)
    const coreTypes = ['Cardano', 'Epoch', 'Slot', 'Block', 'Transaction']
    coreTypes.forEach(t => {
      const gqlType = schema.getType('Block')
      if (!gqlType) {
        throw new Error(`Remote schema is missing ${t}`)
      }
    })
  }, 30)

  return makeRemoteExecutableSchema({
    schema: await introspectSchema(link),
    link
  })
}
