module.exports = function () {
  return {
    files: [
      'src/**/*.ts',
      '!src/**/*.spec.ts'
    ],

    tests: [
      'src/**/*.spec.ts'
    ],
    env: {
      type: 'node'
    },
    testFramework: 'mocha'
  }
}
