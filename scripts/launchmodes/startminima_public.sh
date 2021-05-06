#!/bin/sh

#The current time
current_time=$(date "+%Y.%m.%d-%H.%M.%S")

#Create a filename for each instance
new_fileName1="9001_minilogs_"$current_time.txt
new_fileName2="10001_minilogs_"$current_time.txt
new_fileName3="11001_minilogs_"$current_time.txt

#Make sure folder exists
mkdir -p ~/minilogs

#Run it in the background
nohup java -Xmx3G -jar ~/Minima/jar/minima.jar -port 9001 -conf ~/minimaconf1 -daemon -connect 10.164.0.3 9001 -connect 10.164.0.8 9001 > ~/minilogs/$new_fileName &
sleep 2

#Run it in the background
nohup java -Xmx3G -jar ~/Minima/jar/minima.jar -port 10001 -conf ~/minimaconf2 -daemon -connect 10.164.0.3 9001 -connect 10.164.0.8 9001 > ~/minilogs/$new_fileName &
sleep 2

#Run it in the background
nohup java -Xmx3G -jar ~/Minima/jar/minima.jar -port 11001 -conf ~/minimaconf3 -daemon -connect 10.164.0.3 9001 -connect 10.164.0.8 9001 > ~/minilogs/$new_fileName &
sleep 2

