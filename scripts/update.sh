#!/usr/bin/expect -f

set timeout -1

set launchtype ~/Minima/scripts/launchmodes/startminima_[lindex $argv 1].sh 

#Log into the machine..
spawn gcloud compute ssh [lindex $argv 0]
expect "$ "

#Stop the current version of Minima
send "~/Minima/scripts/stopminima.sh\r"
expect "$ "

#GIT pull the latest version
send "cd ~/Minima\r"
expect "$ "

send "git pull\r"
expect "$ "

#Start the new Minima
send "$launchtype\r"
expect "$ "

send "exit\r"
expect "$ "

#ALL DONE
