# Architecture

## Domain Modelled with GraphQL
GraphQL allows the domain to be modelled in an expressive way, agnostic to the programming language that exposes it for consumption. The language can be serialized for network transmission, hashed for assurance checking, and is suited for describing complex graph-based domains. TypeScript definitions are then generated using [GraphQL Code Generator](https://graphql-code-generator.com/) which are used throughout this implementation, and [will be made available as a package](https://github.com/input-output-hk/cardano-graphql/issues/8) for client consumption. [Other targets](https://graphql-code-generator.com/docs/plugins/) can be included in our build process for automated cross-language tooling.

## Data access abstracted
A repository interface is defined for each entity to decouple the data source from query resolution. Data can then be fetched and cached with optimizations specific to the source, and running context. The Postgres implementations will use the DataLoader pattern to optimize sql queries across resolvers, whereas for demonstration and testing, a simple in-memory store is often suited.

## Application-level caching
If needed this will be implemented as an optimization.