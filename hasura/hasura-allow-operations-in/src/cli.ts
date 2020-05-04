#!/usr/bin/env node
import { run } from './run';

const sourcePath = process.argv[2];
const hasuraUri = process.argv[3];
const allowIntrospection = process.argv[4] === 'true';

if (sourcePath === undefined) {
  throw new Error('Source path must be passed as first argument');
}

if (hasuraUri === undefined) {
  throw new Error('Hasura URI must be passed as the second argument');
}

run(hasuraUri, sourcePath, allowIntrospection)
  .then(result => {
    const operationText =
      result.operationsFound > 1 ? 'operations were' : 'operation was';
    console.log(
      `${result.operationsFound} ${operationText} found and included in the allow list`
    );
  })
  .catch(error => console.error(error));
