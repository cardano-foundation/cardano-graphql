module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      'src/**/*.graphql',
      '!src/**/*.test.ts'
    ],

    tests: [
      'src/**/*.test.ts'
    ],
    env: {
      type: 'node'
    },
    workers: {
      restart: true
    },
    testFramework: 'jest',
  }
}
