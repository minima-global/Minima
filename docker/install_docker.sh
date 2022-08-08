#!/bin/sh

#This script will install docker on a Debian based server

# Remove any old versions
sudo apt-get remove docker docker-engine docker.io containerd runc

# Update Package Manager
sudo apt-get update

#Install the required packages
sudo apt-get -y install ca-certificates curl gnupg lsb-release

#Add Docker's official GPG key
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg

#Set up the Docker repo
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

#Now install Docker - after updating repos again
sudo apt-get update
sudo apt-get -y install docker-ce docker-ce-cli containerd.io docker-compose-plugin

#Start at boot
sudo systemctl enable docker.service
sudo systemctl enable containerd.service