module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      'src/**/*.graphql',
      '!src/**/*.spec.ts'
    ],

    tests: [
      'src/**/*.spec.ts',
      '!src/**/integration.spec.ts'
    ],
    env: {
      type: 'node'
    },
    testFramework: 'mocha'
  }
}
