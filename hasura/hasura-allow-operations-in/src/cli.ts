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
  .then(
    ({
      introspectionAllowed,
      operationDefinitionsFound,
      addedCount,
      existingCount,
    }) => {
      console.log(
        `Introspection allowed: ${introspectionAllowed} | Found: ${operationDefinitionsFound.length} | Added: ${addedCount} | Existing: ${existingCount}`
      );
      if (process.env.DEBUG) {
        operationDefinitionsFound.forEach(def =>
          console.log(`${def.operation}: ${def.name.value}`)
        );
      }
    }
  )
  .catch(error => console.error(error));
