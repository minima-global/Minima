#!/bin/sh

#Logon to a GCLOUD instance and run a function..
echo $1
gcloud compute ssh $2 $3 -- $4
