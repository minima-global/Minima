#!/bin/sh

#Stop using the good way
curl 127.0.0.1:9002/quit
sleep 2

curl 127.0.0.1:10002/quit
sleep 2

curl 127.0.0.1:11002/quit
sleep 2

#Stop using the hard way
kill $(ps aux | grep minima | awk '{print $2}')

echo Minima Stopped!


