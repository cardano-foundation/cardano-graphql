module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      'src/**/*.graphql',
      '!src/**/*.spec.ts'
    ],

    tests: [
      'src/**/*.spec.ts'
    ],
    env: {
      type: 'node'
    },
    workers: {
      restart: true
    },
    testFramework: 'mocha',
  }
}
