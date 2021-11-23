#!/bin/sh
set -e

CLEAN_FLAG=''
PORT=''
HOST=''
HOME="/home/minima"
CONNECTION_HOST=''
CONNECTION_PORT=''
SLEEP=''
RPC=''

print_usage() {
  printf "Usage: Setups a new minima service for the specified port, default 9121 \n \t -c REQUIRED connection host and port HOST:PORT \n \t -u flag Use unsecure p2p version with rpc ports active \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121 \n \t -h minima home directory eg -h /home/minima \n \t -a use the p2p alphas \n"
}

while getopts ':xrsc::p:d:h:' flag; do
  case "${flag}" in
    s) SLEEP='true';;
    x) CLEAN_FLAG='true';;
    r) RPC='true';;
    c) CONNECTION_HOST=$(echo $OPTARG | cut -f1 -d:);
       CONNECTION_PORT=$(echo $OPTARG | cut -f2 -d:);;
    p) PORT="${OPTARG}";;
    d) HOME="${OPTARG}";;
    h) HOST="${OPTARG}";;
    *) print_usage
       exit 1 ;;
  esac
done

apt update
apt install openjdk-11-jre-headless -y

if [ ! $(getent group minima) ]; then
  echo "[+] Adding minima group"
  groupadd -g 9001 minima
fi

if ! id -u 9001 > /dev/null 2>&1; then
  echo "[+] Adding minima user"
    useradd -r -u 9001 -g 9001 -d $HOME minima
    mkdir $HOME
    chown minima:minima $HOME
fi

wget -q -O $HOME"/minima_service.sh" "https://github.com/minima-global/Minima/raw/release-0.100/scripts/minima_service.sh"
chown minima:minima $HOME"/minima_service.sh"
chmod +x $HOME"/minima_service.sh"

CMD="$HOME/minima_service.sh -s $@"
CRONSTRING="#!/bin/sh
$CMD"

echo "$CRONSTRING" > /etc/cron.daily/minima_$PORT
chmod a+x /etc/cron.daily/minima_$PORT

CMD="$HOME/minima_service.sh $@"
/bin/sh -c "$CMD"