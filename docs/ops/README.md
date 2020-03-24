# Ops

There are three supported deployment methods, with varying setup requirements. Service configuration for Cardano GraphQL includes:
- [CORS](./CORS.md)

## Docker
### Prerequisites
- [Docker](https://docs.docker.com/install/)
- [Docker-compose](https://docs.docker.com/compose/install/)

The [docker-compose.yaml](../../docker-compose.yml) can be used as a starting point for your own deployment, checked into source 
control, and further configured to meet your requirements. [cgql](../../cli/README.md) can assist with setting this up,
including secrets provisioning
```
npm install cardano-graphql-cli -g
cgql --help
```


## NixOS
_Todo_

## Node.js
```
yarn  
yarn start
```