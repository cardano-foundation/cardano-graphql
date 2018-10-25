import { multi } from 'getenv';

export default Object.assign({}, multi({
  SWAGGER_SCHEMA_URI: ['SWAGGER_SCHEMA_URI', undefined, 'url'],
  // Todo: Switch to 'url' type when swagger-to-graphql package properly resolves a trailing slash
  REST_ENDPOINT: ['REST_ENDPOINT', undefined, 'string'],
  PORT: ['PORT', 3000, 'string'],
  TLS_PATH_CA: ['TLS_PATH_CA', '/data/auth/ca.cert', 'string'],
  TLS_PATH_KEY: ['TLS_PATH_KEY', '/data/auth/client.key', 'string'],
  TLS_PATH_CERT: ['TLS_PATH_CERT', '/data/auth/client.pem', 'string']
}), {
  get isProduction() { return process.env.NODE_ENV !== 'development' }
});
