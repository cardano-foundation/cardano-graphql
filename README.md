# Cardano GraphQL
[![Build Status](https://jenkins.daedalus-operations.com/buildStatus/icon?job=cardano-graphql%2Fmaster)](https://jenkins.daedalus-operations.com/blue/organizations/jenkins/cardano-graphql/)

Cross-platform, _typed_, and **queryable** API service for Cardano, powered by [Hasura](https://hasura.io/) to precisely query projected Cardano state from [PostgreSQL](https://www.postgresql.org/).

The [schema](src_temp/schema.graphql) is defined in native `.graphql`, and used to generate [packages](src/generated_packages/README.md) for internal and client-side static typing. [Apollo Server](https://www.apollographql.com/docs/apollo-server/) exposes the NodeJS execution engine over a HTTP endpoint. 

**GraphQL** is a query language and execution environment with server and client implementations across many programming languages. The language can be serialized for network transmission, schema implementations hashed for assurance, and is suited for describing most domains.
 
**TypeScript** (and JS) has the largest pool of production-ready libraries, developers, and interoperability in the GraphQL and web ecosystem in general. TypeScript definitions for the schema is generated using [GraphQL Code Generator](https://graphql-code-generator.com), is used within this implementation and [will be made available as a package](https://github.com/input-output-hk/cardano-graphql/issues/8) for client consumption. [Other targets](https://graphql-code-generator.com/docs/plugins/) can be included in our build process for automated cross-language tooling.

## Documentation
Integrated query UI and documentation is available in the GraphQL Playground, available at http://localhost:3100 when running `yarn start:test-stack`.

- [API Consumer](./docs/api_consumer)
- [Developer](./docs/developer)
- [Maintainer](./docs/maintainer)
- [QA](./docs/qa)

