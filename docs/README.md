# Documentation
 - [API Consumer](./api_consumer)
 - [Developer](./developer)
 
**GraphQL** is a query language and execution environment with server and client implementations across many programming languages. The language can be serialized for network transmission, schema implementations hashed for assurance, and is suited for describing most domains.
 
**TypeScript** (and JS) has the largest pool of production-ready libraries, developers, and interoperability in the GraphQL and web ecosystem in general. TypeScript definitions for the schema is generated using [GraphQL Code Generator](https://graphql-code-generator.com), is used within this implementation and [will be made available as a package](https://github.com/input-output-hk/cardano-graphql/issues/8) for client consumption. [Other targets](https://graphql-code-generator.com/docs/plugins/) can be included in our build process for automated cross-language tooling.

 
 ## ENVs
 ### `API_PORT`: Int
Defaults to a random port in development, required for production.

 ### `MOCK_RESPONSES`: Boolean
Configure the `ApolloServer` to [return mock responses](https://www.apollographql.com/docs/apollo-server/features/mocking/)