import graphQLSchema from '@rhyslbw/swagger-to-graphql';
import GraphQLServer from './GraphQLServer';

export default async (swaggerSchema, restEndpoint, options) => {
  const schema = await graphQLSchema(swaggerSchema, restEndpoint, options);
  return new GraphQLServer(schema, options);
};
