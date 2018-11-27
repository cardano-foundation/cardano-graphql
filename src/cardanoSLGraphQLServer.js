import graphQLSchema from 'swagger-to-graphql'
import { mergeSchemas } from 'graphql-tools'
import subscriptionsSchema from './pub-sub/subscriptions'
import GraphQLServer from './GraphQLServer'

export default async (swaggerSchema, restEndpoint, pubSub, options) => {
  const v1Schema = await graphQLSchema(swaggerSchema, restEndpoint, options)
  const subscriptions = subscriptionsSchema(pubSub);
  const schema = mergeSchemas({
    schemas: [v1Schema, subscriptions]
  })
  return new GraphQLServer(schema, options)
}
