# Developer

Review to level concepts in [project architecture](./architecture.md) first.

The project uses a [GitFlow](https://danielkummer.github.io/git-flow-cheatsheet/) process, so all features and fixes must be aimed at `develop` via a Pull Request.

This project is setup to support a [Wallaby](https://wallabyjs.com/) integrated development experience, or test run on file save TDD workflow. You run **one** of the following yarn scripts running during development: 

## Choose a development workflow

### Wallaby `yarn dev` + IDE configuration
With your IDE managing test execution running in the Wallaby process, this script just performs a background task of auto-transpiling TS from the GraphQL schema.

### TDD `yarn dev:test`
A [multiplex](https://www.npmjs.com/package/stmux) terminal with the code generator task, services, tsc watcher, and jest watcher.

## Starting the app `yarn dev:start`
Services and a [ts-node-dev](https://www.npmjs.com/package/ts-node-dev) watcher

## Integration test strategy
It's convenient to use the Apollo Integration test server during development, particularly with Wallaby, as it serves the GraphQL server without booting a HTTP server. The same tests are run on the CI server to provide end-to-end assurance, using an instance of ApolloClient and the API running in a [Docker container](../../test/docker-compose.yml).

