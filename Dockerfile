ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION} as haskell-builder
ARG CABAL_VERSION=3.2.0.0
ARG GHC_VERSION=8.10.2
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app
RUN apt-get update -y && apt-get install -y \
  automake=1:1.16.* \
  build-essential=12.* \
  g++=4:9.3.* \
  git=1:2.25.* \
  jq \
  libffi-dev=3.* \
  libghc-postgresql-libpq-dev=0.9.4.* \
  libgmp-dev=2:6.2.* \
  libncursesw5=6.* \
  libpq-dev=12.* \
  libssl-dev=1.1.* \
  libsystemd-dev=245.* \
  libtinfo-dev=6.* \
  libtool=2.4.* \
  make=4.2.* \
  pkg-config=0.29.* \
  tmux=3.* \
  wget=1.20.* \
  zlib1g-dev=1:1.2.*
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-x86_64-unknown-linux.tar.xz &&\
  tar -xf cabal-install-${CABAL_VERSION}-x86_64-unknown-linux.tar.xz &&\
  rm cabal-install-${CABAL_VERSION}-x86_64-unknown-linux.tar.xz cabal.sig &&\
  mv cabal /usr/local/bin/
RUN cabal update
WORKDIR /app/ghc
RUN wget --secure-protocol=TLSv1_2 \
  https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-deb9-linux.tar.xz &&\
  tar -xf ghc-${GHC_VERSION}-x86_64-deb9-linux.tar.xz &&\
  rm ghc-${GHC_VERSION}-x86_64-deb9-linux.tar.xz
WORKDIR /app/ghc/ghc-${GHC_VERSION}
RUN ./configure && make install
WORKDIR /app/src
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium &&\
  git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV}
WORKDIR /app/src/libsodium
RUN ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
WORKDIR /app/src
COPY /cardano-node /app/src/cardano-node
WORKDIR /app/src/cardano-node
RUN cabal install cardano-cli \
  --install-method=copy \
  --installdir=/usr/local/bin \
  -f -systemd
# Cleanup for server copy of /usr/local/lib
RUN rm -rf /usr/local/lib/ghc-${GHC_VERSION} /usr/local/lib/pkgconfig

FROM ubuntu:${UBUNTU_VERSION} as ubuntu-nodejs
ARG NODEJS_MAJOR_VERSION=14
ENV DEBIAN_FRONTEND=nonintercative
RUN apt-get update && apt-get install curl -y &&\
  curl --proto '=https' --tlsv1.2 -sSf -L https://deb.nodesource.com/setup_${NODEJS_MAJOR_VERSION}.x | bash - &&\
  apt-get install nodejs -y

FROM ubuntu-nodejs as nodejs-builder
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

FROM nodejs-builder as cardano-graphql-builder
RUN yarn --offline --frozen-lockfile --non-interactive &&\
   yarn build

FROM nodejs-builder as cardano-graphql-production-deps
RUN yarn --offline --frozen-lockfile --non-interactive --production

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30 as downloader
RUN apk add curl
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | sh
ENV HASURA_GRAPHQL_ENABLE_TELEMETRY=false
RUN hasura --skip-update-check update-cli --version v1.3.3

FROM nodejs-builder as dev
RUN apt-get update && apt-get install yarn -y
RUN mkdir src
RUN mkdir /node-ipc
COPY --from=haskell-builder /usr/local/lib /usr/local/lib
COPY --from=haskell-builder /usr/local/bin/cardano-cli /usr/local/bin/
COPY --from=downloader /usr/local/bin/hasura /usr/local/bin/hasura
ENV \
  CARDANO_CLI_PATH=/usr/local/bin/cardano-cli \
  CARDANO_NODE_CONFIG_PATH=/config/cardano-node/config.json \
  CARDANO_NODE_SOCKET_PATH=/node-ipc/node.socket \
  GENESIS_FILE_BYRON=/config/genesis/byron.json \
  GENESIS_FILE_SHELLEY=/config/genesis/shelley.json \
  HASURA_CLI_PATH=/usr/local/bin/hasura \
  HASURA_URI="http://hasura:8080" \
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
  POSTGRES_DB_FILE=/run/secrets/postgres_db \
  POSTGRES_HOST=postgres \
  POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password \
  POSTGRES_PORT=5432 \
  POSTGRES_USER_FILE=/run/secrets/postgres_user
WORKDIR /src
  
FROM ubuntu-nodejs as server
ARG NETWORK=mainnet 
ARG METADATA_SERVER_URI="https://tokens.cardano.org/metadata"
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - &&\
  echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" | tee  /etc/apt/sources.list.d/pgdg.list &&\
  apt-get update && apt-get install -y --no-install-recommends \
  ca-certificates \
  jq
COPY --from=haskell-builder /usr/local/lib /usr/local/lib
COPY --from=haskell-builder /usr/local/bin/cardano-cli /usr/local/bin/
COPY --from=downloader /usr/local/bin/hasura /usr/local/bin/hasura
ENV \
  CARDANO_CLI_PATH=/usr/local/bin/cardano-cli \
  CARDANO_NODE_CONFIG_PATH=/config/cardano-node/config.json \
  CARDANO_NODE_SOCKET_PATH=/node-ipc/node.socket \
  GENESIS_FILE_BYRON=/config/genesis/byron.json \
  GENESIS_FILE_SHELLEY=/config/genesis/shelley.json \
  HASURA_CLI_PATH=/usr/local/bin/hasura \
  HASURA_GRAPHQL_ENABLE_TELEMETRY=false \
  HASURA_URI="http://hasura:8080" \
  JQ_PATH=/usr/bin/jq \
  LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
  METADATA_SERVER_URI=${METADATA_SERVER_URI} \
  NETWORK=${NETWORK} \
  POSTGRES_DB_FILE=/run/secrets/postgres_db \
  POSTGRES_HOST=postgres \
  POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password \
  POSTGRES_PORT=5432 \
  POSTGRES_USER_FILE=/run/secrets/postgres_user
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
COPY config/network/${NETWORK}/genesis /config/genesis/
RUN mkdir /node-ipc
WORKDIR /app/packages/server/dist
EXPOSE 3100
CMD ["node", "index.js"]
