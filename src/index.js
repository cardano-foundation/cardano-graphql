import fs from 'fs';
import request from 'request-promise-native';
import cardanoSLGraphQLServer from './cardanoSLGraphQLServer';
import config from './config'

const {
  SWAGGER_SCHEMA_URI,
  REST_ENDPOINT,
  TLS_PATH_CA,
  TLS_PATH_KEY,
  TLS_PATH_CERT,
  PORT,
  isProduction
} = config;

const startServer = async (options) => {
  const swaggerSchema = JSON.parse(await request({
    uri: SWAGGER_SCHEMA_URI.href,
    agentOptions: options.agentOptions
  }));
  const apiServer = await cardanoSLGraphQLServer(swaggerSchema, REST_ENDPOINT, options);
  apiServer.listen({ port: PORT }, () => {
    console.log(`🚀 Server ready at ${apiServer.endpoint(PORT)}`);
  });
};

const hasTlsAuth = () => new Promise((resolve, reject) => {
  fs.access(TLS_PATH_CA, (error) => {
    if (error && error.code === 'ENOENT') {
      return resolve(false);
    } else if (error) {
      return reject(error);
    }
    resolve(true);
  });
});

const apiAvailable = (agentOptions) => new Promise((resolve, reject) => {
  request({
    uri: SWAGGER_SCHEMA_URI,
    resolveWithFullResponse: true,
    agentOptions
  }).then(({ statusCode }) => resolve(statusCode === 200)).catch((error) => {
    if (error.cause.code === 'ECONNREFUSED') return resolve(false);
    reject(error);
  });
});

let attemptCount = 0;
const retry = () => {
  const DELAY_BETWEEN_CHECKS = 2000;
  const MAX_RETRIES = 20;
  if (attemptCount >= MAX_RETRIES) throw new Error(`Can not connect to Wallet API after ${attemptCount} attempts`);
  setTimeout(initialize, DELAY_BETWEEN_CHECKS);
};

const initialize = async () => {
  attemptCount++;
  // Wait until Wallet REST API certificate is provisioned
  try {
    if (await hasTlsAuth()) {
      const options = {
        agentOptions: {
          ca: fs.readFileSync(TLS_PATH_CA),
          key: fs.readFileSync(TLS_PATH_KEY),
          cert: fs.readFileSync(TLS_PATH_CERT),
          rejectUnauthorized: isProduction
        }
      };
      if (await apiAvailable(options.agentOptions)) return startServer(options);
    }
    retry();
  } catch (error) {
    console.error(error.message);
    throw error;
  }
};

initialize();
