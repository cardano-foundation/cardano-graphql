FROM node:10.15.3-alpine as builder
RUN apk add --update python make g++ yarn
RUN mkdir /application
COPY package.json yarn.lock /application/
WORKDIR /application
RUN mkdir /application/packages
COPY tsconfig.json /application/
COPY packages/api-cardano-db-hasura /application/packages/api-cardano-db-hasura
COPY packages/server /application/packages/server
COPY packages/util /application/packages/util
COPY packages/util-dev /application/packages/util-dev
RUN yarn --pure-lockfile --non-interactive
RUN yarn workspaces run build

FROM node:10.15.3-alpine as production_deps
RUN mkdir -p application/packages
COPY package.json yarn.lock /application/
WORKDIR /application/packages
RUN mkdir api-cardano-db-hasura util server
COPY packages/api-cardano-db-hasura/package.json api-cardano-db-hasura/
COPY packages/server/package.json server/
COPY packages/util/package.json util/
WORKDIR /application
RUN yarn --production --pure-lockfile --non-interactive

FROM node:10.15.3-alpine as server
RUN mkdir /application
COPY --from=builder /application/packages/api-cardano-db-hasura/dist /application/packages/api-cardano-db-hasura/dist
COPY --from=builder /application/packages/api-cardano-db-hasura/package.json /application/packages/api-cardano-db-hasura/package.json
COPY --from=builder /application/packages/api-cardano-db-hasura/schema.graphql /application/packages/api-cardano-db-hasura/schema.graphql
COPY --from=builder /application/packages/server/dist /application/packages/server/dist
COPY --from=builder /application/packages/server/package.json /application/packages/server/package.json
COPY --from=builder /application/packages/util/dist /application/packages/util/dist
COPY --from=builder /application/packages/util/package.json /application/packages/util/package.json
COPY --from=production_deps /application/node_modules /application/node_modules
WORKDIR /application
COPY --from=production_deps /application/packages/api-cardano-db-hasura/node_modules /application/packages/api-cardano-db-hasura/node_modules
COPY --from=production_deps /application/packages/server/node_modules /application/packages/server/node_modules
WORKDIR /application/packages/server/dist
EXPOSE 3100
CMD ["node", "index.js"]
