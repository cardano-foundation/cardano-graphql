FROM node:10.15.3-alpine as builder
RUN apk add --update python make g++ yarn
RUN mkdir -p /application/src
COPY package.json yarn.lock codegen.yml codegen.external.yml /application/
COPY src/schema.graphql /application/src
WORKDIR /application
RUN yarn
COPY . /application
RUN yarn build

FROM node:10.15.3-alpine as production_deps
RUN mkdir /application
COPY package.json yarn.lock /application/
WORKDIR /application
RUN yarn --production

FROM node:10.15.3-alpine as server
RUN mkdir /application
COPY --from=builder /application/dist /application
COPY --from=production_deps /application/node_modules /application/node_modules
WORKDIR /application
CMD ["node", "index.js"]
