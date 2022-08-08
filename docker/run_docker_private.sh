#!/bin/sh

#Get the USER
baseuser=`id -u`

#Get the Group
basegroup=`id -g`

#WHo are we running as
echo Running as .. $(id -un)

#Create a new folder - as current user
mkdir ~/minidocker_private

#Remove the old container
sudo docker rm minimaprivate

#Start her up in interactive mode
sudo docker run -it -e minima_daemon=false -e minima_nop2p=true -e minima_test=true -e minima_genesis=true --user $baseuser:$basegroup -v ~/minidocker_private:/home/minima/data --name minimaprivate minimaglobal/minima:latest