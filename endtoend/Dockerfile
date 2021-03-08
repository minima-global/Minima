#FROM keymetrics/pm2:latest-alpine
FROM node:15-alpine
#FROM node:10
#FROM mhart/alpine-node:15

WORKDIR /app/

# install some packages on Alpine
RUN apk --no-cache add \
    python \
    bash

# Copy nodejs package files
COPY package-lock.json package.json /app/

# Install packages
#ENV NPM_CONFIG_LOGLEVEL warn
# copy most files here so that all previous layers can be cached longer
RUN npm ci 
COPY src /app/src


# Expose the listening port of your app
# For dev, expose API port
#EXPOSE 8545

CMD ["node", "src/index.js"]


