const fs = require('fs');
const path = require('path');
const { loadFilesSync } = require('@graphql-tools/load-files');
const { mergeTypeDefs } = require('@graphql-tools/merge');
const { printSchemaWithDirectives } = require('@graphql-tools/utils');
const { makeExecutableSchema } = require('@graphql-tools/schema');

const cardanoDbHasuraSchema = loadFilesSync(path.resolve(__dirname, '..', 'api', 'cardano-db-hasura', 'schema.graphql'));
const genesisSchema = loadFilesSync(path.resolve(__dirname, '..', 'api', 'genesis', 'schema.graphql'));
const mergedTypes = mergeTypeDefs([cardanoDbHasuraSchema, genesisSchema], {
  throwOnConflict: true
});

const executableSchema = makeExecutableSchema({ typeDefs: [mergedTypes] });
const schema = printSchemaWithDirectives(executableSchema);

fs.writeFileSync(path.resolve(__dirname, '..','api', 'schema.graphql'), schema);
