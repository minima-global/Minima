#!/bin/bash
# This script assumes image minima-latest exists locally
# This can be verified with command: docker images

# WARNING: this script depends on docker and jq (json command line parsing tool)

# config
DOCKER_NET="minima-e2e-testnet"

# stop old containers
docker stop minima-node-01 minima-node-02 minima-node-03
# Delete old containers
docker rm   minima-node-01 minima-node-02 minima-node-03
# delete network
docker network rm $DOCKER_NET

echo -e "\n   All docker minima nodes stopped.\n\n"
 
