#!/usr/bin/expect -f

set timeout -1

#Log into the machine..
spawn gcloud compute ssh [lindex $argv 0] [lindex $argv 1] 
expect "$ "

#GIT pull the latest version
send "cd ~/Minima\r"
expect "$ "

send "git pull\r"
expect "$ "

send "exit\r"
expect "$ "

#ALL DONE
