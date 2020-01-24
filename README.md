# Cardano GraphQL
[![Build Status](https://jenkins.daedalus-operations.com/buildStatus/icon?job=cardano-graphql%2Fdevelop)](https://jenkins.daedalus-operations.com/blue/organizations/jenkins/cardano-graphql/)

Cross-platform, _typed_, and **queryable** API service for Cardano, powered by [Hasura](https://hasura.io/) to precisely query projected Cardano state from [PostgreSQL](https://www.postgresql.org/).

The [schema](src_temp/schema.graphql) is defined in native `.graphql`, and used to generate [packages](src/generated_packages/README.md) for internal and client-side static typing. [Apollo Server](https://www.apollographql.com/docs/apollo-server/) exposes the NodeJS execution engine over a HTTP endpoint. 

## Documentation
Integrated query UI and documentation is available in the GraphQL Playground, available at http://localhost:3100 when running `yarn start:test-stack`.

- [API Consumer Documentation](./docs/api_consumer)
- [Developer Documentation](./docs/developer)
- [Updating Hasura Metadata](./docs/hasura)
- [Refresh Test DB Dump](./docs/refresh_db)

