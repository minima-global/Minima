#!/bin/sh

#First update the main server
./update.sh minima-private-1-vm --zone=europe-west4-a
./update.sh minima-private-2-vm --zone=europe-west4-a

#Run through the public servers..
./update.sh minima-public-1-vm --zone=europe-west4-a
./update.sh minima-public-2-vm --zone=europe-west4-a
./update.sh minima-public-3-vm --zone=europe-west4-a
./update.sh minima-public-4-vm --zone=europe-west4-a
./update.sh minima-public-5-vm --zone=europe-west4-a
./update.sh minima-public-6-vm --zone=europe-west4-a

#Update the miner..
./update.sh minima-miner-1-vm --zone=europe-west2-a
./update.sh minima-miner-2-vm --zone=europe-west2-a
./update.sh minima-miner-3-vm --zone=europe-west2-a
./update.sh minima-miner-4-vm --zone=europe-west2-a
