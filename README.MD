# Cardano GraphQL Server
[![JavaScript Style Guide](https://cdn.rawgit.com/standard/standard/master/badge.svg)](https://github.com/standard/standard)

NodeJS implementation built using [Apollo Server](https://www.apollographql.com/docs/apollo-server/) 🚀, with schema generated from the published Cardano SL [Swagger](https://swagger.io/) spec.

**WARNINGS**
– This is a development release, and therefore the API may change.
– The service provides an adaptor for the Cardano Wallet REST API, and (currently) does not offer isolation from upstream changes.

## Configuration
Options [here](src/config.js). Cardano SL _edge_ nodes expose the API documentation which includes the swagger.json at _/docs/v1/swaggger.json_

## Deployment
### Docker
Use the [published image](https://hub.docker.com/r/rhyslbw/cardano-graphql) or build your own.
### Build and run from source
```
npm build && npm run build
```

## Development
```
docker-compose up
```
This sets up a stack with a demo Cardano SL cluster. Explore the schema in the [GraphQL Playground](http://localhost:4000/graphql). The Dockerfile copies the compiled source from `/dist`, so for now run `yarn build` before building the image.

### Planned features
- [ ] Connection status subscription



