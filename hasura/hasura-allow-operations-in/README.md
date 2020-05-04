# hasura-allow-operations-in
Populate the Hasura allow-list from found GraphQL operations in a path, including queries, mutations, and subscriptions.
Optionally include the introspection query by passing `true` as the third argument.

## Run with [npx](https://nodejs.dev/the-npx-nodejs-package-runner)
```
npx hasura-allow-operations-in '**/*.graphql' http://localhost:8090 true
```

## Global install from [npm](https://www.npmjs.com/package/hasura-allow-operations-in)
```
npm i -g hasura-allow-operations-in
hasura-allow-operations-in http://localhost:8090 '**/*.graphql' true

## cleanup
npm uninstall -g hasura-allow-operations-in
```

## ECMAScript + `.d.ts`
```
import { run, RunReport } from 'hasura-allow-operations-in'

run('http://localhost:8090', '**/*.graphql', true)
  .then((result: RunReport) => {
    console.log(`At least ${result.operationsFound} are in the allow list`);
  })
  .catch(error => console.error(error));
```

## dev
```
HASURA_URI=http://localhost:8090 yarn && yarn dev '**/*.graphql' http://localhost:8090 true
```
