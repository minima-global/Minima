#!/bin/sh

old="$IFS"
IFS='/'
str="'$*'"
COMMAND="'$*'"
curl http://127.0.0.1:9002/$COMMAND
IFS=$old