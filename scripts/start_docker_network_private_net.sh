#!/bin/bash
# This script assumes image minima-latest exists locally
# This can be verified with command: docker images

# WARNING: this script depends on docker and jq (json command line parsing tool)

# Config 
#  you can set another image name here
DOCKER_IMAGE="minima:latest"
# you can change docker minima network name here
DOCKER_NET="minima-e2e-testnet"
# you can change default delay between nodes start here (default 5 s)
DOCKER_WAIT_START=5
# defaults: all nodes connect in star topology to node 1 (we do not use docker dns as minima does not yet support it)
# docker assign new ip to node 1, so we start node 1 early and look up using jq its IP address
DOCKER_MINIMA_START_NODE_01="-private -clean"
#DOCKER_MINIMA_START_NODE_02="-connect 172.26.0.2 9001"
#DOCKER_MINIMA_START_NODE_03="-connect 172.26.0.2 9001"
#DOCKER_MINIMA_START_NODE_02="-connect minima-node-01 9001"
#DOCKER_MINIMA_START_NODE_03="-connect minima-node-01 9001"
# TODO: number of nodes and loop

# First we stop existing minima docker instances
docker stop minima-node-01 minima-node-02 minima-node-03
# Delete old containers
docker rm   minima-node-01 minima-node-02 minima-node-03
# delete network
docker network rm $DOCKER_NET

# create new network
docker network create $DOCKER_NET
docker create --name minima-node-01 --network $DOCKER_NET $DOCKER_IMAGE $DOCKER_MINIMA_START_NODE_01
echo "Starting node 01 early..."
docker start minima-node-01
# retrieve node 1 ip
DOCKER_NODE01_IP=`docker inspect minima-node-01  | jq -r '.[0].NetworkSettings.Networks."minima-e2e-testnet".IPAddress'`
echo "Node 01 IP address is: $DOCKER_NODE01_IP."
DOCKER_MINIMA_START_NODE_02="-connect $DOCKER_NODE01_IP 9001"
DOCKER_MINIMA_START_NODE_03="-connect $DOCKER_NODE01_IP 9001"
docker create --name minima-node-02 --network $DOCKER_NET $DOCKER_IMAGE $DOCKER_MINIMA_START_NODE_02
docker create --name minima-node-03 --network $DOCKER_NET $DOCKER_IMAGE $DOCKER_MINIMA_START_NODE_03

# start all instances
#echo "Starting node 01..."
#docker start minima-node-01
sleep $DOCKER_WAIT_START
echo "Starting node 02..."
docker start minima-node-02
sleep $DOCKER_WAIT_START
echo "Starting node 03..."
docker start minima-node-03
sleep $DOCKER_WAIT_START

# node01 should see two connections
NODE01_CONNECTIONS=`docker exec minima-node-01 curl -s $DOCKER_NODE01_IP:9002/status | jq '.response.connections'`
if [[ "$NODE01_CONNECTIONS" -eq 0 ]] ; then
    echo " *** ERROR *** node01 has 0 connections.";
    exit;
fi

if [ $NODE01_CONNECTIONS -eq 2 ] ; then
   echo -e "\n   SUCCESSS Network ready to use.\n"
   echo -e "   Try checking node status: docker exec minima-node-01 curl -s $DOCKER_NODE01_IP:9002/status | jq \n\n"

else
   echo " INCOMPLETE Unexpected connections number: $NODE01_CONNECTIONS";
fi
 
