#!/bin/sh

#PRIVATE COMPUTERS
./logon.sh PRIVATE1 minima-private-1-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PRIVATE2 minima-private-2-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 

#PUBLIC COMPUTERS
./logon.sh PUBLIC1 minima-public-1-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PUBLIC2 minima-public-2-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PUBLIC3 minima-public-3-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PUBLIC4 minima-public-4-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PUBLIC5 minima-public-5-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh PUBLIC6 minima-public-6-vm --zone=europe-west4-a "crontab -l > mycronbackup.txt && crontab -r" 

#MINERS
./logon.sh MINER1 minima-miner-1-vm --zone=europe-west2-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh MINER2 minima-miner-2-vm --zone=europe-west2-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh MINER3 minima-miner-3-vm --zone=europe-west2-a "crontab -l > mycronbackup.txt && crontab -r" 
./logon.sh MINER4 minima-miner-4-vm --zone=europe-west2-a "crontab -l > mycronbackup.txt && crontab -r" 
