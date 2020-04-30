import { GraphQLSchema } from 'graphql'
import { buildHasuraSchema } from './buildHasuraSchema'

export type Context = () => ({
  hasura: GraphQLSchema
})

export async function buildContext (hasuraUri: string): Promise<Context> {
  const hasura = await buildHasuraSchema(hasuraUri)
  return () => ({ hasura })
}
