import axios, { AxiosError } from 'axios';
import { QueryCollection } from './index';

const config = { headers: { 'X-Hasura-Role': 'admin' } };

function throwIfUnexpected(error: AxiosError): void {
  if (error.response.data.code !== 'already-exists') throw error;
}

export async function api(
  hasuraUri: string,
  collectionQueries: QueryCollection[]
): Promise<void> {
  const uri = `${hasuraUri}/v1/query`;
  const collectionName = 'allowed-queries';
  try {
    await axios.post(
      uri,
      {
        type: 'create_query_collection',
        args: {
          name: collectionName,
          definition: {
            queries: collectionQueries,
          },
        },
      },
      config
    );
  } catch (error) {
    throwIfUnexpected(error);
    // The collection exists, but the contents are unknown
    // Ensure each query is in the allow list
    for (const collectionQuery of collectionQueries) {
      try {
        await axios.post(
          uri,
          {
            type: 'add_query_to_collection',
            args: {
              collection_name: collectionName, // eslint-disable-line @typescript-eslint/camelcase
              query_name: collectionQuery.name, // eslint-disable-line @typescript-eslint/camelcase
              query: collectionQuery.query,
            },
          },
          config
        );
      } catch (error) {
        throwIfUnexpected(error);
      }
    }
  }
}
