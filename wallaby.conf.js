module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      'src/**/*.graphql',
      '!src/**/integration.test.ts'
    ],

    tests: [
      'src/**/integration.test.ts'
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
