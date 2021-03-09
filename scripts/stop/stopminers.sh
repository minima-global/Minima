#!/bin/sh

#./stopnet.sh minima-private-1-vm --zone=europe-west4-a
#./stopnet.sh minima-private-2-vm --zone=europe-west4-a

./stopnet.sh minima-miner-1-vm --zone=europe-west2-a
./stopnet.sh minima-miner-2-vm --zone=europe-west2-a
./stopnet.sh minima-miner-3-vm --zone=europe-west2-a
./stopnet.sh minima-miner-4-vm --zone=europe-west2-a

#./stopnet.sh minima-public-1-vm  --zone=europe-west4-a
#./stopnet.sh minima-public-2-vm  --zone=europe-west4-a
#./stopnet.sh minima-public-3-vm  --zone=europe-west4-a
#./stopnet.sh minima-public-4-vm  --zone=europe-west4-a
#./stopnet.sh minima-public-5-vm  --zone=europe-west4-a
#./stopnet.sh minima-public-6-vm  --zone=europe-west4-a
