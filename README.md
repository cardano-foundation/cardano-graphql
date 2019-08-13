# Cardano GraphQL
[![Build Status](http://13.238.211.79:8080/buildStatus/icon?job=cardano-graphql%2Fdevelop)](http://13.238.211.79:8080/blue/organizations/jenkins/cardano-graphql/)

This product aims to a provide an API for all features of Cardano, including the graph-based models such as the UTXO ledger. The [schema](./src/schema.graphql) is highly portable, and is used to generate [packages](src/generated_packages/README.md) to use within implementations. This repository contains a N implementation [Apollo Server](https://www.apollographql.com/docs/apollo-server/), and generated using [GraphQL Code Generator](https://graphql-code-generator.com/)., and can be implemented across any number of contexts by mapping resolvers to fields, and executing based on the [spec]. A network API over TCP is the core implementation In addition, this repository contains a NodeJS execution runtime, HTTP Server, and definitions to deploy a production-ready instance to container-based hosting scenarios.


## Project State: Alpha
The project is in rapid active development, so don't use in production yet.

## Documentation
Integrated query UI and documentation is available in the GraphQL Playground, available from the a local dev environment. A hosted version of the Playground [will be provided](https://github.com/input-output-hk/cardano-graphql/issues/9)
- [API Consumer Documentation](./docs/api_consumer)
- [Developer Documentation](./docs/developer)

