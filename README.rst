.. raw:: html

   <p align="center">
     <a href="https://jenkins.daedalus-operations.com/blue/organizations/jenkins/cardano-graphql/"><img src="https://jenkins.daedalus-operations.com/buildStatus/icon?job=cardano-graphql%2Fmaster&style=flat-square" /></a>
     <a href="https://github.com/input-output-hk/cardano-graphql/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-graphql.svg?style=for-the-badge" /></a>
   </p>

****************************
``cardano-graphql`` Overview
****************************

Cross-platform, *typed*, and **queryable** API for Cardano. The project
contains multiple `packages`_ for composing GraphQL services to meet
specific application demands, and a `docker-compose stack`_ serving the
included `cardano-graphql-server Dockerfile`_ and the extended `hasura
Dockerfile`_. The `schema`_ is defined in native ``.graphql``, and used
to generate a TypeScript `package`_ for client-side static typing.

`Apollo Server`_ exposes the NodeJS execution engine over a HTTP
endpoint, and includes support for open source metrics via Prometheus,
and implementing operation filtering to deny unexpected queries. Should
you wish to have more control over the server, or stitch the schema with
an existing service, consider importing the executable schema from the
``@cardano-graphql/api-*`` packages only.

**GraphQL** is a query language and execution environment with server
and client implementations across many programming languages. The
language can be serialized for network transmission, schema
implementations hashed for assurance, and is suited for describing most
domains.

**TypeScript** (and JS) has the largest pool of production-ready
libraries, developers, and interoperability in the GraphQL and web
ecosystem in general. TypeScript definitions for the schema, generated
by `GraphQL Code Generator`_, are available on `npm`_.

.. _packages: ./packages
.. _docker-compose stack: ./docker-compose.yml
.. _cardano-graphql-server Dockerfile: ./Dockerfile
.. _hasura Dockerfile: ./packages/api-cardano-db-hasura/hasura/Dockerfile
.. _schema: packages/api-cardano-db-hasura/schema.graphql
.. _package: packages/client-ts/README.md
.. _Apollo Server: https://www.apollographql.com/docs/apollo-server/
.. _GraphQL Code Generator: https://graphql-code-generator.com
.. _npm: https://www.npmjs.com/package/cardano-graphql-ts

Documentation
-------------

+-------------------------------+----------------------------------+
|             Link              |             Audience             |
+===============================+==================================+
| `API Documentation`_          | Users of the Cardano GraphQL API |
+-------------------------------+----------------------------------+
| `Wiki`_                       | Anyone interested in the project |
|                               | and our development process      |
+-------------------------------+----------------------------------+
| `Example Queries - Cardano DB | Users of the Cardano DB Hasura   |
| Hasura`_                      | API                              |
+-------------------------------+----------------------------------+

.. _API Documentation: https://input-output-hk.github.io/cardano-graphql
.. _Wiki: https://github.com/input-output-hk/cardano-graphql/wiki
.. _Example Queries - Cardano DB Hasura: ./packages/api-cardano-db-hasura/src/example_queries