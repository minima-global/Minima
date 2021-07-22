#FROM keymetrics/pm2:latest-alpine
FROM node:16-alpine
#FROM node:10
#FROM mhart/alpine-node:15

WORKDIR /app/

# install some packages on Alpine
RUN apk --no-cache add \
    python2 \
    bash \
    build-base \
    g++ \
    cairo-dev \
    jpeg-dev \
    pango-dev \
    giflib-dev

RUN apk add --update  --repository http://dl-3.alpinelinux.org/alpine/edge/testing libmount ttf-dejavu ttf-droid ttf-freefont ttf-liberation ttf-ubuntu-font-family fontconfig

# Copy nodejs package files
COPY package-lock.json package.json minima-api-1.0.0.tgz /app/

# Install packages
#ENV NPM_CONFIG_LOGLEVEL warn
# copy most files here so that all previous layers can be cached longer
RUN npm ci 
COPY src /app/src


# Expose the listening port of your app
# For dev, expose API port
#EXPOSE 8545

CMD ["node", "src/index.js"]

ARG topology=star
ARG nbNodes=3
ARG nodeFailure=2
ARG graph=false

ENV topology=$topology
ENV nbNodes=$nbNodes
ENV nodeFailure=$nodeFailure
ENV graph=$graph

RUN echo $topology
RUN echo $nbNodes
RUN echo $nodeFailure
RUN echo $graph
