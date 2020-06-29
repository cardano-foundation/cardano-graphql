module.exports = function () {
  return {
    files: [
      'packages/api-cardano-db-hasura/src/**/*.ts',
      'packages/api-cardano-db-hasura/**/*.graphql',
      '!packages/api-cardano-db-hasura/src/**/*.test.ts'
    ],
    tests: [
      'packages/api-cardano-db-hasura/src/**/*.test.ts'
    ],
    env: {
      params: {
        env: 'TEST_MODE=integration'
      },
      type: 'node'
    },
    workers: {
      restart: true
    },
    testFramework: 'jest'
  }
}
