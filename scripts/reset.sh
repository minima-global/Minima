#!/bin/sh

./logon.sh PUBLIC1 minima-public-1-vm --zone=europe-west4-a "cd ~ && sudo apt -y install tmux openjdk-8-jre git htop && rm -rf ~/Minima && rm -rf ~/.minima && git clone -b development https://github.com/spartacusrex99/Minima.git" 


#./logon.sh MINER1 minima-miner-1-vm --zone=europe-west2-a "cd ~ && sudo apt -y install tmux openjdk-8-jre git htop && rm -rf ~/Minima && rm -rf ~/.minima && git clone -b development https://github.com/spartacusrex99/Minima.git" 
#./logon.sh MINER2 minima-miner-2-vm --zone=europe-west2-a "cd ~ && sudo apt -y install tmux openjdk-8-jre git htop && rm -rf ~/Minima && rm -rf ~/.minima && git clone -b development https://github.com/spartacusrex99/Minima.git" 
#./logon.sh MINER3 minima-miner-3-vm --zone=europe-west2-a "cd ~ && sudo apt -y install tmux openjdk-8-jre git htop && rm -rf ~/Minima && rm -rf ~/.minima && git clone -b development https://github.com/spartacusrex99/Minima.git" 
#./logon.sh MINER4 minima-miner-4-vm --zone=europe-west2-a "cd ~ && sudo apt -y install tmux openjdk-8-jre git htop && rm -rf ~/Minima && rm -rf ~/.minima && git clone -b development https://github.com/spartacusrex99/Minima.git" 