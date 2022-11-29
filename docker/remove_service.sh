#!/bin/sh
set -e

#Remove The OLD Minima install
CLEAN_FLAG=''
PORT=''
HOME="/home/minima"


print_usage() {
  printf "Usage: Uninstalls minima: \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121"
}

while getopts ':xp:' flag; do
  case "${flag}" in
    x) CLEAN_FLAG='true';;
    p) PORT="${OPTARG}";;
    *) print_usage
       exit 1 ;;
  esac
done


echo "Stopping minima service"
systemctl stop minima_$PORT
echo "Disabling minima service"
systemctl disable minima_$PORT

echo "Removing /etc/cron.weekly/minima_$PORT"
rm /etc/cron.weekly/minima_$PORT
rm /etc/systemd/system/minima_$PORT.service
systemctl daemon-reload
systemctl reset-failed

echo "Removing $HOME/minima_service.sh"
rm $HOME"/minima_service.sh"
echo "Removing $HOME/minima.jar"
rm $HOME"/minima.jar"

if [ $CLEAN_FLAG ]; then
 echo "Removing data directory $HOME/.minima_$PORT"
 rm -rf $HOME/.minima_$PORT
fi


