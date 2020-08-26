#!/bin/sh

#First update the main server
./update.sh minima-private-1-vm private
#sleep 300
./update.sh minima-private-2-vm private
#sleep 300

#Update the miner..
./update.sh minima-miner-1-vm miner
#sleep 300
./update.sh minima-miner-2-vm miner

#Run through the public servers..
./update.sh minima-public-1-vm public
#sleep 60
./update.sh minima-public-2-vm public
#sleep 60
./update.sh minima-public-3-vm public
#sleep 60
./update.sh minima-public-4-vm public
#sleep 60


