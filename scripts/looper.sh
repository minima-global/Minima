until ./starter.sh; do
    echo "Server 'myserver' crashed with exit code $?.  Respawning.." >&2
    sleep 3
done
