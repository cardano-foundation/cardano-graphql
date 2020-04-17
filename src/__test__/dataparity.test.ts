import * as tests from './dataparitytests'

const { TEST_MODE } = process.env
if (!TEST_MODE || (TEST_MODE !== 'e2e' && TEST_MODE !== 'integration')) {
  throw new Error('Test mode must be provided (e2e || integration)')
}

const createClient = TEST_MODE === 'e2e'
  ? require('./e2e_client').createClient
  : require('./integration_client').createClient

Object.values(tests).forEach(test => test(createClient))
