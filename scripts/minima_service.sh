#!/bin/sh
set -e
PATH=/sbin:/bin:/usr/bin

P2P_ALPHA=''
UNSECURE_FLAG=''
CLEAN_FLAG=''
PORT=''
HOME="/home/minima"
LOCAL="/usr/local"
CONNECTION_HOST=''
CONNECTION_PORT=''
SLEEP=''

print_usage() {
  printf "Usage: Setups a new minima service for the specified port, default 9121 \n \t -c REQUIRED connection host and port HOST:PORT \n \t -u flag Use unsecure p2p version with rpc ports active, ignored if -a isn't also set \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121 \n \t -h minima home directory eg -h /home/minima \n \t -a use the p2p alphas \n"
}

while getopts ':uxsac::p:h:' flag; do
  case "${flag}" in
    a) P2P_ALPHA='true';;
    s) SLEEP='true';;
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
if [ $SLEEP ]; then
  #Random Pause up to 1 hr
  sleep $[$RANDOM % 3600]
fi

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


CURL_RETURN_CODE=0
curl --fail {127.0.0.1:$PORT/quit} 2> /dev/null || CURL_RETURN_CODE=$?
if [ ${CURL_RETURN_CODE} -eq 0 ];
  then
    # Give minima time to shutdown
    echo "[!] Shutting down Minima"
    sleep 40
fi

if [ ! $PORT ]; then
  if [ $P2P_ALPHA ]; then
    PORT='9121'
  else
    PORT='9001'
  fi
fi

DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/master/jar/minima.jar"
MINIMA_JAR_NAME="minima.jar"
if [ $P2P_ALPHA ]; then
    echo "[!] Using P2P Alpha version of minima"
    DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/feature_p2p/jar/minima-p2p-alpha.jar"
    MINIMA_JAR_NAME="minima-p2p-alpha.jar"

fi

echo "[+] Downloading minima from: $DOWNLOAD_URL"
wget -q -O $LOCAL"/"$MINIMA_JAR_NAME $DOWNLOAD_URL
chown minima:minima $LOCAL"/"$MINIMA_JAR_NAME
chmod +x $LOCAL"/"$MINIMA_JAR_NAME

if [ ! -d "$HOME/.minima_$PORT" ]; then
  echo "[+] Creating config directory .minima_${PORT}..."
  mkdir $HOME/.minima_$PORT
  chown minima:minima $HOME/.minima_$PORT
fi


is_service_exists() {
    local x=$1
    if systemctl status "${x}" 2> /dev/null | grep -Fq "Active:"; then
            return 0
    else
            return 1
    fi
}

if is_service_exists "minima_$PORT"; then
  echo "[!] Disabling minima service"
  systemctl stop minima_$PORT
  systemctl disable minima_$PORT
fi


echo "[+] Creating service minima_$PORT"

MINIMA_PARAMS="-daemon -test -port $PORT -conf $HOME/.minima_$PORT"
if [ $CLEAN_FLAG ]; then
  MINIMA_PARAMS="$MINIMA_PARAMS -clean"
fi

if [ $P2P_ALPHA ]; then
    if [ ! $UNSECURE_FLAG ]; then
      MINIMA_PARAMS="$MINIMA_PARAMS -secure"
    fi
    MINIMA_JAR_NAME="minima-p2p-alpha.jar"
  else
    MINIMA_JAR_NAME="minima.jar"
fi

if [ $CONNECTION_PORT ]; then
  MINIMA_PARAMS="$MINIMA_PARAMS -connect $CONNECTION_HOST $CONNECTION_PORT"
fi
tee <<EOF >/dev/null /etc/systemd/system/minima_$PORT.service
[Unit]
Description=minima_$PORT
[Service]
User=minima
Type=simple
ExecStart=/usr/bin/java -Xmx1G -jar $LOCAL/$MINIMA_JAR_NAME $MINIMA_PARAMS
Restart=always
RestartSec=100
[Install]
WantedBy=multi-user.target

EOF


systemctl daemon-reload
systemctl enable minima_$PORT
systemctl start minima_$PORT