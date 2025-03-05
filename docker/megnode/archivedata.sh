#!/bin/sh

#Get the currentn date
dt=$(date '+%Y_%m_%d_%H%M%S');

#FIRST DO THE MYSQL
#Create a filename based on the time..
filename=minima_mysql_$dt.raw.dat
exportfile=/home/minima/backups/mysql/$filename

#Run a command on Minima node..
curl minima:9005/mysql+file:$exportfile+action:rawexport 

#Delete files that are older than 21 days
find /home/minima/backups/mysql/ -type f -mtime +21 -delete

#NOW DO THE MEGAMMR
#Create a filename based on the time..
filenamemmr=minima_megammr_$dt.mmr
exportfilemmr=/home/minima/backups/megammr/$filenamemmr

#Run a command on Minima node..
curl minima:9005/megammr+file:$exportfilemmr+action:export 

#Delete files that are older than 21 days
find /home/minima/backups/megammr/ -type f -mtime +21 -delete

