import { AxiosError } from 'axios';
import { getIntrospectionQuery, OperationDefinitionNode } from 'graphql';
import { loadDocuments } from '@graphql-toolkit/core';
import { GraphQLFileLoader } from '@graphql-toolkit/graphql-file-loader';
import {
  init,
  fromToolkitSource,
  QueryCollection,
} from './Hasura/schema_metadata/QueryCollection';

export type RunReport = {
  addedCount: number;
  collectionCreated: boolean;
  existingCount: number;
  introspectionAllowed: boolean;
  operationDefinitionsFound: OperationDefinitionNode[];
};

function throwIfUnexpected(error: AxiosError): void {
  if (
    error.response === undefined ||
    error.response.data.code !== 'already-exists'
  )
    throw error;
}

export async function run(
  hasuraUri: string,
  sourcePaths: string | string[],
  allowIntrospection?: boolean
): Promise<RunReport> {
  const definitionNodes: OperationDefinitionNode[] = [];
  const sources = await loadDocuments(sourcePaths, {
    loaders: [new GraphQLFileLoader()],
  });
  sources.forEach(source => {
    source.document.definitions.forEach(def =>
      definitionNodes.push(def as OperationDefinitionNode)
    );
  });
  const collectionItem: QueryCollection[] = sources.map(source =>
    fromToolkitSource(source)
  );
  if (allowIntrospection)
    collectionItem.push({
      name: 'IntrospectionQuery',
      query: getIntrospectionQuery(),
    });
  const report: RunReport = {
    addedCount: 0,
    existingCount: 0,
    collectionCreated: false,
    introspectionAllowed: allowIntrospection,
    operationDefinitionsFound: definitionNodes,
  };
  const api = init(hasuraUri);
  try {
    await api.createQueryCollection(collectionItem);
    report.collectionCreated = true;
    report.addedCount = collectionItem.length;
  } catch (error) {
    throwIfUnexpected(error);
    // The collection exists, but the contents are unknown
    // Ensure each query is in the allow list
    for (const item of collectionItem) {
      try {
        await api.addQueryToCollection(item);
        report.addedCount++;
      } catch (error) {
        throwIfUnexpected(error);
        report.existingCount++;
      }
    }
  }
  return report;
}
