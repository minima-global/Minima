#!/bin/sh

#Create a filename based on the time..
filename=minima_archive_$(date +"%Y_%m_%d-%T").gzip
exportfile=/home/minima/backups/$filename

#Run a command on Minima node..
curl minima:9005/mysql+file:$exportfile+host:mysql:3306+database:archivedb+user:archiveuser+password:archivepassword+action:h2export 