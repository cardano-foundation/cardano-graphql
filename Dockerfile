FROM node:10.15.3-alpine as builder
RUN apk add --update python make g++ yarn
RUN mkdir /application
COPY package.json yarn.lock .yarnrc /application/
COPY packages-cache /application/packages-cache
WORKDIR /application
RUN mkdir /application/packages
COPY tsconfig.json /application/
COPY packages/api-cardano-db-hasura /application/packages/api-cardano-db-hasura
COPY packages/server /application/packages/server
COPY packages/util /application/packages/util
COPY packages/util-dev /application/packages/util-dev
RUN yarn --offline --frozen-lockfile --non-interactive
RUN yarn build

FROM node:10.15.3-alpine as production_deps
RUN mkdir -p application/packages
COPY package.json yarn.lock .yarnrc /application/
COPY --from=builder /application/packages-cache /application/packages-cache
WORKDIR /application/packages
RUN mkdir api-cardano-db-hasura util server
COPY packages/api-cardano-db-hasura/package.json api-cardano-db-hasura/
COPY packages/server/package.json server/
COPY packages/util/package.json util/
WORKDIR /application
RUN yarn --production --offline --frozen-lockfile --non-interactive

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30 as server
RUN apk add curl nodejs
RUN mkdir /application
COPY --from=builder /application/packages/api-cardano-db-hasura/dist /application/packages/api-cardano-db-hasura/dist
COPY --from=builder /application/packages/api-cardano-db-hasura/hasura/project /application/packages/api-cardano-db-hasura/hasura/project
COPY --from=builder /application/packages/api-cardano-db-hasura/package.json /application/packages/api-cardano-db-hasura/package.json
COPY --from=builder /application/packages/api-cardano-db-hasura/schema.graphql /application/packages/api-cardano-db-hasura/schema.graphql
COPY --from=builder /application/packages/server/dist /application/packages/server/dist
COPY --from=builder /application/packages/server/package.json /application/packages/server/package.json
COPY --from=builder /application/packages/util/dist /application/packages/util/dist
COPY --from=builder /application/packages/util/package.json /application/packages/util/package.json
COPY --from=production_deps /application/node_modules /application/node_modules
RUN curl -L https://github.com/hasura/graphql-engine/raw/stable/cli/get.sh | sh
RUN hasura update-cli --version v1.2.1
WORKDIR /application
WORKDIR /application/packages/server/dist
EXPOSE 3100
CMD ["node", "index.js"]
