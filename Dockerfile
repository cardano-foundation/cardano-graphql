FROM node:10.15.3-alpine as builder
RUN mkdir /application
COPY package.json /application/package.json
WORKDIR /application
RUN npm i
COPY . /application
RUN npm run build

FROM node:10.15.3-alpine as production_deps
RUN mkdir /application
COPY package.json /application/package.json
WORKDIR /application
RUN npm i --production

FROM node:10.15.3-alpine as server
RUN mkdir /application
COPY --from=builder /application/dist /application
COPY --from=production_deps /application/node_modules /application/node_modules
WORKDIR /application
CMD ["node", "index.js"]
