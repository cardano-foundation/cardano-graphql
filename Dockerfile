FROM node:10.15.3-alpine as nodejs_builder
RUN apk add --update python make g++ yarn
RUN mkdir /application
COPY package.json yarn.lock .yarnrc /application/
COPY packages-cache /application/packages-cache
WORKDIR /application
RUN mkdir /application/packages
COPY tsconfig.json /application/
COPY packages/api-cardano-db-hasura /application/packages/api-cardano-db-hasura
COPY packages/api-cardano-node /application/packages/api-cardano-node
COPY packages/api-genesis /application/packages/api-genesis
COPY packages/server /application/packages/server
COPY packages/util /application/packages/util
COPY packages/util-dev /application/packages/util-dev
RUN yarn --offline --frozen-lockfile --non-interactive
RUN yarn build

FROM node:10.15.3-alpine as nodejs_production_deps
RUN mkdir -p application/packages
COPY package.json yarn.lock .yarnrc /application/
COPY --from=nodejs_builder /application/packages-cache /application/packages-cache
WORKDIR /application/packages
RUN mkdir api-cardano-db-hasura api-cardano-node api-genesis util server
COPY packages/api-cardano-db-hasura/package.json api-cardano-db-hasura/
COPY packages/api-cardano-node/package.json api-cardano-node/
COPY packages/api-genesis/package.json api-genesis/
COPY packages/server/package.json server/
COPY packages/util/package.json util/
WORKDIR /application
RUN yarn --production --offline --frozen-lockfile --non-interactive

FROM debian:buster-slim as cardano_haskell_builder
ARG CARDANO_NODE_TAG=1.15.1
WORKDIR /build
RUN apt -yq update && apt -yq upgrade

RUN apt install build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsodium-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 \
    -y
RUN wget https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz && \
    tar -xf cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz && \
    rm cabal-install-3.2.0.0-x86_64-unknown-linux.tar.xz cabal.sig && \
    mv cabal /usr/local/bin/
RUN cabal update
WORKDIR /app/ghc
RUN wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb9-linux.tar.xz && \
    tar -xf ghc-8.6.5-x86_64-deb9-linux.tar.xz && \
    rm ghc-8.6.5-x86_64-deb9-linux.tar.xz
WORKDIR /app/ghc/ghc-8.6.5
RUN ./configure
RUN make install
WORKDIR /app
RUN mkdir build
RUN git clone https://github.com/input-output-hk/cardano-node.git && \
  cd cardano-node && \
  git fetch --all --tags && \
  git checkout ${CARDANO_NODE_TAG}
WORKDIR /app/cardano-node
COPY config/cabal.project.local .
RUN cabal build cardano-cli && \
  mv ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/cardano-cli-${CARDANO_NODE_TAG}/x/cardano-cli/build/cardano-cli/cardano-cli /usr/local/bin/

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30 as downloader
RUN apk add curl
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | sh
RUN hasura --skip-update-check update-cli --version v1.2.1

#FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30 as server
#RUN apt add nodejs
FROM debian:buster-slim as server
RUN apt -yq update && apt -yq upgrade
RUN apt install nodejs -y
RUN mkdir /application
COPY --from=cardano_haskell_builder /usr/lib /usr/lib
COPY --from=cardano_haskell_builder /etc /etc
COPY --from=cardano_haskell_builder /usr/local/bin/cardano-cli /usr/local/bin/cardano-cli
COPY --from=nodejs_builder /application/packages/api-cardano-db-hasura/dist /application/packages/api-cardano-db-hasura/dist
COPY --from=nodejs_builder /application/packages/api-cardano-db-hasura/hasura/project /application/packages/api-cardano-db-hasura/hasura/project
COPY --from=nodejs_builder /application/packages/api-cardano-db-hasura/package.json /application/packages/api-cardano-db-hasura/package.json
COPY --from=nodejs_builder /application/packages/api-cardano-db-hasura/schema.graphql /application/packages/api-cardano-db-hasura/schema.graphql
COPY --from=nodejs_builder /application/packages/api-cardano-node/dist /application/packages/api-cardano-node/dist
COPY --from=nodejs_builder /application/packages/api-cardano-node/package.json /application/packages/api-cardano-node/package.json
COPY --from=nodejs_builder /application/packages/api-cardano-node/schema.graphql /application/packages/api-cardano-node/schema.graphql
COPY --from=nodejs_builder /application/packages/api-genesis/dist /application/packages/api-genesis/dist
COPY --from=nodejs_builder /application/packages/api-genesis/package.json /application/packages/api-genesis/package.json
COPY --from=nodejs_builder /application/packages/api-genesis/schema.graphql /application/packages/api-genesis/schema.graphql
COPY --from=nodejs_builder /application/packages/server/dist /application/packages/server/dist
COPY --from=nodejs_builder /application/packages/server/package.json /application/packages/server/package.json
COPY --from=nodejs_builder /application/packages/util/dist /application/packages/util/dist
COPY --from=nodejs_builder /application/packages/util/package.json /application/packages/util/package.json
COPY --from=nodejs_production_deps /application/node_modules /application/node_modules
COPY --from=downloader /usr/local/bin/hasura /usr/local/bin/hasura
WORKDIR /application
WORKDIR /application/packages/server/dist
EXPOSE 3100
CMD ["node", "index.js"]
