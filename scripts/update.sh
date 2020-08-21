#!/usr/bin/expect -f

set timeout -1

#Log into the machine..
spawn gcloud compute ssh [lindex $argv 0]
expect "$ "

#Wait a second..
send "sleep 1\r"
expect "$ "

#Jump Home
send "cd ~\r"
expect "$ "

#Stop the current version of Minima
send "~/Minima/scripts/stopminima.sh\r"
expect "$ "

send "sleep 1\r"
expect "$ "

#GIT pull the latest version
send "cd Minima\r"
expect "$ "

send "git pull\r"
expect "$ "

send "cd ~\r"
expect "$ "

#Start the new Minima
send "~/Minima/scripts/startminima.sh\r"
expect "$ "

send "sleep 1\r"
expect "$ "

send "exit\r"
expect "$ "

#ALL DONE
