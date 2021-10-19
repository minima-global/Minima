#!/bin/sh
set -e

UNSECURE_FLAG=''
CLEAN_FLAG=''
PORT='9121'
HOME="/home/minima"
LOCAL="/usr/local"

print_usage() {
  printf "Usage: Setups a new minima service for the specified port, default 9121 \n \t -c REQUIRED connection host and port HOST:PORT \n \t -u flag Use unsecure p2p version with rpc ports active \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121 \n \t -h minima home directory eg -h /home/minima \n \t -a use the p2p alphas \n"
}

while getopts ':uxp:h:' flag; do
  case "${flag}" in
    u) UNSECURE_FLAG='true';;
    x) CLEAN_FLAG='true';;
    p) PORT="${OPTARG}";;
    h) HOME="${OPTARG}";;
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

if  ! cat /etc/systemd/journald.conf | grep "Storage=persistent"; then
tee <<EOF >/dev/null /etc/systemd/journald.conf
Storage=persistent
EOF
systemctl restart systemd-journald
fi

echo "[!] Using P2P Alpha version of minima"
DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/feature_p2p/jar/minima-p2p-alpha.jar"
MINIMA_JAR_NAME="minima-p2p-alpha.jar"

wget -q -O $LOCAL"/minima_service.sh" "https://github.com/minima-global/Minima/raw/feature_p2p/scripts/minima_service.sh"
chown minima:minima $LOCAL"/minima_service.sh"
chmod +x $LOCAL"/minima_service.sh"

CMD="$LOCAL/minima_service.sh -s -a -c 34.89.151.186:9121 $@"
CRONSTRING="#!/bin/sh
$CMD"

echo "$CRONSTRING" > /etc/cron.daily/minima_$PORT
chmod a+x /etc/cron.daily/minima_$PORT

CMD="$LOCAL/minima_service.sh -a -x -c 34.89.151.186:9121 $@"
/bin/sh -c "$CMD"