# Developer

Review to level concepts in [project architecture](./architecture.md) first.

The project uses a [GitFlow](https://danielkummer.github.io/git-flow-cheatsheet/) process, so all features and fixes must be aimed at `develop` via a Pull Request.

This project is setup to support a [Wallaby](https://wallabyjs.com/) integrated development experience, or test run on file save TDD workflow. You run **one** of the following yarn scripts running during development: 

## Wallaby Workflow: `yarn dev`
With your IDE managing test execution running in the Wallaby process, this script just performs a background task of auto-transpiling TS from the GraphQL schema.

## TDD `yarn dev:test`
A [multiplex](https://www.npmjs.com/package/stmux) terminal with the code generator task, mocha, a tsc watcher for separate static type checking, and eslint watcher.

## Start the app `yarn dev:start`
Same as the TDD script, but uses [ts-node-dev](https://www.npmjs.com/package/ts-node-dev) to run the service instead of the test watcher
