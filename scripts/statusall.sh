#!/bin/sh

#First update the main server
echo "PRIVATE 1"
gcloud compute ssh minima-private-1-vm  -- "~/Minima/scripts/status.sh"
echo "PRIVATE 2"
gcloud compute ssh minima-private-2-vm  -- "~/Minima/scripts/status.sh"

#echo "MINER 1"
#gcloud compute ssh minima-miner-1-vm  -- "~/Minima/scripts/status.sh"
#echo "MINER 2"
#gcloud compute ssh minima-miner-2-vm --zone=europe-west2-a  -- "~/Minima/scripts/status.sh"

echo "PUBLIC 1"
gcloud compute ssh minima-public-1-vm  -- "~/Minima/scripts/status.sh"
echo "PUBLIC 2"
gcloud compute ssh minima-public-2-vm  -- "~/Minima/scripts/status.sh"
echo "PUBLIC 3"
gcloud compute ssh minima-public-3-vm  -- "~/Minima/scripts/status.sh"
echo "PUBLIC 4"
gcloud compute ssh minima-public-4-vm  -- "~/Minima/scripts/status.sh"
echo "PUBLIC 5"
gcloud compute ssh minima-public-5-vm  -- "~/Minima/scripts/status.sh"
echo "PUBLIC 6"
gcloud compute ssh minima-public-6-vm  -- "~/Minima/scripts/status.sh"
