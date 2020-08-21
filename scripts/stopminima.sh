#!/bin/sh

#Stop using the good way
curl 127.0.0.1:9002/quit

#Give it a second
sleep 5

#Stop using the hard way
kill $(ps aux | grep minima | awk '{print $2}')

echo Minima Stopped!


