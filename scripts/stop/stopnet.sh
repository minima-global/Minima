#!/usr/bin/expect -f

set timeout -1

#Log into the machine..
spawn gcloud compute ssh [lindex $argv 0] [lindex $argv 1]
expect "$ "

#Stop the current version of Minima
send "~/Minima/scripts/stopminima.sh\r"
expect "$ "

send "exit\r"
expect "$ "

#ALL DONE
