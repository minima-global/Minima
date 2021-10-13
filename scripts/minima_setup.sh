#!/bin/sh
set -e

P2P_ALPHA=''
UNSECURE_FLAG=''
CLEAN_FLAG=''
PORT='9121'
HOME="/home/minima"
LOCAL="/usr/local"
CONNECTION_HOST=''
CONNECTION_PORT=''

print_usage() {
  printf "Usage: Setups a new minima service for the specified port, default 9121 \n \t -c REQUIRED connection host and port HOST:PORT \n \t -u flag Use unsecure p2p version with rpc ports active \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121 \n \t -h minima home directory eg -h /home/minima \n \t -a use the p2p alphas \n"
}

while getopts ':auxc::p:h:' flag; do
  case "${flag}" in
    a) P2P_ALPHA='true';;
    u) UNSECURE_FLAG='true';;
    x) CLEAN_FLAG='true';;
    c) CONNECTION_HOST=$(echo $OPTARG | cut -f1 -d:);
       CONNECTION_PORT=$(echo $OPTARG | cut -f2 -d:);;
    p) PORT="${OPTARG}";;
    h) HOME="${OPTARG}";;
    *) print_usage
       exit 1 ;;
  esac
done

sudo apt update
sudo apt install openjdk-11-jre-headless -y

if [ ! $(getent group minima) ]; then
  echo "[+] Adding minima group"
  sudo groupadd -g 9001 minima
fi

if ! id -u 9001 > /dev/null 2>&1; then
  echo "[+] Adding minima user"
    sudo useradd -r -u 9001 -g 9001 -d $HOME minima
    sudo mkdir $HOME
    sudo chown minima:minima $HOME
fi

if  ! cat /etc/systemd/journald.conf | grep "Storage=persistent"; then
sudo tee <<EOF >/dev/null /etc/systemd/journald.conf
Storage=persistent
EOF
sudo systemctl restart systemd-journald
fi

DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/master/jar/minima.jar"
MINIMA_JAR_NAME="minima.jar"
if [ $P2P_ALPHA ];
  then
    echo "[!] Using P2P Alpha version of minima"
    DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/feature_p2p/jar/minima-p2p-alpha.jar"
    MINIMA_JAR_NAME="minima-p2p-alpha.jar"
fi

sudo sudo wget -q -O $LOCAL"/minima_service.sh" "https://github.com/minima-global/Minima/raw/feature_p2p/scripts/minima_service.sh"
sudo chown minima:minima $LOCAL"/minima_service.sh"
sudo chmod +x $LOCAL"/minima_service.sh"


RANDOM_MIN=$(shuf -i10-59 -n1)
CRONSTRING="$RANDOM_MIN 1 * * *  $LOCAL/minima_service.sh $@"


CRONSTRING="#!/bin/sh\n$LOCAL/minima_service.sh $@"

sudo echo "$CRONSTRING" >> /etc/cron.daily/minima
sudo chmod a+x /etc/cron.daily/minima

