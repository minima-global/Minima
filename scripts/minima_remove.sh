#!/bin/sh
set -e

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




echo "Removing /etc/cron.daily/minima_$PORT"
rm -f /etc/cron.daily/minima_$PORT


echo "Removing /etc/cron.weekly/minima_$PORT"
rm -f /etc/cron.weekly/minima_$PORT



rm /etc/systemd/system/minima_$PORT.service
systemctl daemon-reload
systemctl reset-failed

echo "Removing $HOME/minima_service.sh"
rm -f $HOME"/minima_service.sh"


if [ $CLEAN_FLAG ]; then
 echo "Removing data directory $HOME/.minima_$PORT"
 rm -rf $HOME/.minima_$PORT
fi


