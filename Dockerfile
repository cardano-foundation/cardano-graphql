ARG UBUNTU_VERSION=20.04

FROM ubuntu:${UBUNTU_VERSION} AS ubuntu-nodejs
ARG NODEJS_MAJOR_VERSION=18
ENV DEBIAN_FRONTEND=nonintercative
RUN apt-get update && apt-get install curl -y &&\
  curl --proto '=https' --tlsv1.2 -sSf -L https://deb.nodesource.com/setup_${NODEJS_MAJOR_VERSION}.x | bash - &&\
  apt-get install nodejs -y

FROM ubuntu-nodejs AS nodejs-builder
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - &&\
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list &&\
  apt-get update && apt-get install gcc g++ make gnupg2 yarn -y
RUN mkdir -p /app/packages
WORKDIR /app
COPY packages-cache packages-cache
COPY packages/api-cardano-db-hasura packages/api-cardano-db-hasura
COPY packages/server packages/server
COPY packages/util packages/util
COPY packages/util-dev packages/util-dev
COPY \
  .yarnrc \
  package.json \
  yarn.lock \
  tsconfig.json \
  /app/

FROM nodejs-builder AS cardano-graphql-builder
RUN yarn --frozen-lockfile --non-interactive &&\
  yarn build

FROM nodejs-builder AS cardano-graphql-production-deps
RUN yarn --frozen-lockfile --non-interactive --production

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30 AS downloader
RUN apk add curl
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | sh
ENV HASURA_GRAPHQL_ENABLE_TELEMETRY=false
RUN hasura --skip-update-check update-cli --version v2.17.1
RUN hasura --skip-update-check plugins install cli-ext

FROM nodejs-builder AS dev
RUN apt-get update && apt-get install yarn -y
RUN mkdir src
RUN mkdir /node-ipc
COPY --from=downloader /usr/local/bin/hasura /usr/local/bin/hasura
ENV \
  CARDANO_NODE_CONFIG_PATH=/config/cardano-node/config.json \
  CARDANO_NODE_SOCKET_PATH=/node-ipc/node.socket \
  HASURA_CLI_PATH=/usr/local/bin/hasura \
  HASURA_URI="http://hasura:8080" \
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
  POSTGRES_DB_FILE=/run/secrets/postgres_db \
  POSTGRES_HOST=postgres \
  POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password \
  POSTGRES_PORT=5432 \
  POSTGRES_USER_FILE=/run/secrets/postgres_user
WORKDIR /src

FROM ubuntu-nodejs AS background
ARG NETWORK=mainnet
# using local token registry as default
ARG METADATA_SERVER_URI="http://token-metadata-registry:8091"
RUN apt-get update -y && apt-get install lsb-release -y
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates \
  && rm -rf /var/lib/apt/lists/*
COPY --from=downloader /usr/local/bin/hasura /usr/local/bin/hasura
COPY --from=downloader  /root/.hasura/plugins/bin/hasura-cli_ext /usr/local/bin/hasura-cli_ext
ENV \
  HASURA_CLI_PATH=/usr/local/bin/hasura \
  HASURA_CLI_EXT_PATH=/usr/local/bin/hasura-cli_ext \
  HASURA_URI="http://hasura:8080" \
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
  METADATA_SERVER_URI=${METADATA_SERVER_URI} \
  OGMIOS_HOST="ogmios" \
  OGMIOS_PORT=1337 \
  POSTGRES_DB_FILE=/run/secrets/postgres_db \
  POSTGRES_HOST=postgres \
  POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password \
  POSTGRES_PORT=5432 \
  POSTGRES_USER_FILE=/run/secrets/postgres_user
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/dist /app/packages/api-cardano-db-hasura/dist
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/hasura/project /app/packages/api-cardano-db-hasura/hasura/project
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/package.json /app/packages/api-cardano-db-hasura/package.json
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/schema.graphql /app/packages/api-cardano-db-hasura/schema.graphql
COPY --from=cardano-graphql-builder /app/packages/util/dist /app/packages/util/dist
COPY --from=cardano-graphql-builder /app/packages/util/package.json /app/packages/util/package.json
COPY --from=cardano-graphql-production-deps /app/node_modules /app/node_modules
COPY --from=cardano-graphql-production-deps /app/packages/api-cardano-db-hasura/node_modules /app/packages/api-cardano-db-hasura/node_modules
WORKDIR /app/packages/api-cardano-db-hasura/dist
CMD ["node", "background.js"]

FROM ubuntu-nodejs AS server
ARG NETWORK=mainnet
ENV \
  CARDANO_NODE_CONFIG_PATH=/config/cardano-node/config.json \
  HASURA_GRAPHQL_ENABLE_TELEMETRY=false \
  HASURA_URI="http://hasura:8080" \
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
  NETWORK=${NETWORK} \
  OGMIOS_HOST="ogmios" \
  OGMIOS_PORT=1337
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/dist /app/packages/api-cardano-db-hasura/dist
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/hasura/project /app/packages/api-cardano-db-hasura/hasura/project
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/package.json /app/packages/api-cardano-db-hasura/package.json
COPY --from=cardano-graphql-builder /app/packages/api-cardano-db-hasura/schema.graphql /app/packages/api-cardano-db-hasura/schema.graphql
COPY --from=cardano-graphql-builder /app/packages/server/dist /app/packages/server/dist
COPY --from=cardano-graphql-builder /app/packages/server/package.json /app/packages/server/package.json
COPY --from=cardano-graphql-builder /app/packages/util/dist /app/packages/util/dist
COPY --from=cardano-graphql-builder /app/packages/util/package.json /app/packages/util/package.json
COPY --from=cardano-graphql-production-deps /app/node_modules /app/node_modules
COPY --from=cardano-graphql-production-deps /app/packages/api-cardano-db-hasura/node_modules /app/packages/api-cardano-db-hasura/node_modules
COPY config/network/${NETWORK}/cardano-node /config/cardano-node/
WORKDIR /app/packages/server/dist
EXPOSE 3100
CMD ["node", "index.js"]

FROM cardanofoundation/cf-token-metadata-registry-api:latest AS token-registry
ADD scripts/token-registry-init.sh /app/entrypoint.sh
ENTRYPOINT sh /app/entrypoint.sh
