#!/bin/sh

#Install tmux git and java
sudo apt -y install tmux openjdk-8-jre git htop 

#Remove the old..
rm -rf ~/Minima

#The old settings...
rm -rf ~/.minima

#Clone the Minima git dir..
git clone -b development https://github.com/spartacusrex99/Minima.git

