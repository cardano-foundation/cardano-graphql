module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      'src/**/*.graphql',
      '!src/**/suite.test.ts'
    ],

    tests: [
      'src/**/suite.test.ts'
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
    testFramework: 'jest',
  }
}
