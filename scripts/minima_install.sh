#!/bin/sh
set -e

UNSECURE_FLAG=''
CLEAN_FLAG='false'
PORT='9121'
HOME="/home/minima"
LOCAL="/usr/local"
CONNECTION_HOST=''
CONNECTION_PORT=''
SHOW_LOGS=''

print_usage() {
  printf "Usage: Setups a new minima service for the specified port, default 9121 \n \t -c REQUIRED connection host and port HOST:PORT \n \t -u flag Use unsecure p2p version with rpc ports active \n \t -x flag enable clean flag \n \t -p minima port to use eg. -p 9121 \n \t -h minima home directory eg -h /home/minima \n \t -l flag show service logs \n"
}

while getopts ':uxlc::p:h:' flag; do
  case "${flag}" in
    u) UNSECURE_FLAG='true';;
    x) CLEAN_FLAG='true';;
    l) SHOW_LOGS='true';;
    c) CONNECTION_HOST=$(echo $OPTARG | cut -f1 -d:);
       CONNECTION_PORT=$(echo $OPTARG | cut -f2 -d:);;
    p) PORT="${OPTARG}";;
    h) HOME="${OPTARG}";;
    *) print_usage
       exit 1 ;;
  esac
done

if [ ! $CONNECTION_PORT ];
then
  echo "You need to specify a connection host and port using for example -c 127.0.0.1:9001"
  exit 1;
fi

#sudo apt update
#sudo apt install openjdk-11-jre-headless -y

DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/feature_p2p/jar/minima-p2p-alpha.jar"
MINIMA_JAR_NAME="minima-p2p-alpha.jar"
if [ $UNSECURE_FLAG ];
  then
    echo "[!] Using Unsecure version of minima, anyone with access to $PORT + 1  can access your wallet"
    DOWNLOAD_URL="https://github.com/minima-global/Minima/raw/feature_p2p/jar/minima-p2p-alpha-unsecure.jar"
    MINIMA_JAR_NAME="minima-p2p-alpha-unsecure.jar"
fi

CURL_RETURN_CODE=0
curl --fail {127.0.0.1:$PORT/quit} 2> /dev/null || CURL_RETURN_CODE=$?
if [ ${CURL_RETURN_CODE} -eq 0 ];
  then
    # Give minima time to shutdown
    echo "[!] Shutting down Minima"
    sleep 40
fi



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

if [ ! -d "$HOME/.minima_$PORT" ]; then
  echo "[+] Creating config directory .minima_${PORT}..."
  sudo mkdir $HOME/.minima_$PORT
  sudo chown minima:minima $HOME/.minima_$PORT
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
  sudo systemctl stop minima_$PORT
  sudo systemctl disable minima_$PORT
fi

echo "[+] Downloading minima from: $DOWNLOAD_URL"
sudo sudo wget -q -O $LOCAL"/"$MINIMA_JAR_NAME $DOWNLOAD_URL
sudo chown minima:minima $LOCAL"/"$MINIMA_JAR_NAME


if  ! cat /etc/systemd/journald.conf | grep "Storage=persistent"; then
sudo tee <<EOF >/dev/null /etc/systemd/journald.conf
Storage=persistent
EOF
sudo systemctl restart systemd-journald
fi

echo "[+] Creating service minima_$PORT"

MINIMA_PARAMS="-daemon -test -connect $CONNECTION_HOST $CONNECTION_PORT -port $PORT -conf $HOME/.minima_$PORT"
if [ $CLEAN_FLAG ]; then
  MINIMA_PARAMS="$MINIMA_PARAMS -clean"
fi
sudo tee <<EOF >/dev/null /etc/systemd/system/minima_$PORT.service
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


sudo systemctl daemon-reload
sudo systemctl enable minima_$PORT
sudo systemctl start minima_$PORT

if [ $SHOW_LOGS ]; then
  journalctl -u minima_$PORT -f
fi