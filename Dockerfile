FROM node:alpine
LABEL maintainer="Rhys Bartels-Waller <rhys.bartelswaller@iohk.io>"
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
# Install dependencies
COPY package.json /usr/src/app
COPY yarn.lock  /usr/src/app
RUN yarn install
# Bundle app dist
COPY ./dist /usr/src/app/
EXPOSE 3000
CMD ["node", "index.js"]
