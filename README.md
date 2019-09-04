# Cardano GraphQL
[![Build Status](http://13.238.211.79:8080/buildStatus/icon?job=cardano-graphql%2Fdevelop)](http://13.238.211.79:8080/blue/organizations/jenkins/cardano-graphql/)

This product aims to a provide a cross-platform, _typed_, and **queryable** API service for Cardano. The [schema](src_temp/schema.graphql) is defined in native `.graphql`, and used to generate [packages](src/generated_packages/README.md) for internal and client-side static typing. [Apollo Server](https://www.apollographql.com/docs/apollo-server/) exposes the NodeJS execution engine over a HTTP endpoint. 

## Documentation
Integrated query UI and documentation is available in the GraphQL Playground, available from the a local dev environment. A hosted version of the Playground [will be provided](https://github.com/input-output-hk/cardano-graphql/issues/9)

- [API Consumer Documentation](./docs/api_consumer)
- [Developer Documentation](./docs/developer)

