#!/bin/sh

#Stop everything first..
#cd stop
#./stopall.sh
#cd ..

#PRIVATE COMPUTERS
./logon.sh PRIVATE1 minima-private-1-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PRIVATE2 minima-private-2-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 

#PUBLIC COMPUTERS
./logon.sh PUBLIC1 minima-public-1-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PUBLIC2 minima-public-2-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PUBLIC3 minima-public-3-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PUBLIC4 minima-public-4-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PUBLIC5 minima-public-5-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh PUBLIC6 minima-public-6-vm --zone=europe-west4-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 

#MINERS
./logon.sh MINER1 minima-miner-1-vm --zone=europe-west2-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh MINER2 minima-miner-2-vm --zone=europe-west2-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh MINER3 minima-miner-3-vm --zone=europe-west2-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 
./logon.sh MINER4 minima-miner-4-vm --zone=europe-west2-a "cd ~ && rm -rf ~/.minima && rm -rf ~/Minima && git clone https://github.com/minima-global/Minima.git" 