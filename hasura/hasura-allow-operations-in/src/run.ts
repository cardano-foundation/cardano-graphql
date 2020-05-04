import { getIntrospectionQuery } from 'graphql';
import { loadDocuments } from '@graphql-toolkit/core';
import { GraphQLFileLoader } from '@graphql-toolkit/graphql-file-loader';
import {
  api,
  fromToolkitSource,
  QueryCollection,
} from './Hasura/schema_metadata/QueryCollection';

export type RunReport = {
  operationsFound: number;
};

export async function run(
  hasuraUri: string,
  sourcePaths: string | string[],
  allowIntrospection: boolean
): Promise<RunReport> {
  const sources = await loadDocuments(sourcePaths, {
    loaders: [new GraphQLFileLoader()],
  });
  const queries: QueryCollection[] = [];
  if (allowIntrospection) {
    queries.push({
      name: 'IntrospectionQuery',
      query: getIntrospectionQuery(),
    });
  }
  await api(
    hasuraUri,
    queries.concat(sources.map(source => fromToolkitSource(source)))
  );
  return { operationsFound: sources.length };
}
