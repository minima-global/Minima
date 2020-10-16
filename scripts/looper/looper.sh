until ./starter.sh; do
    echo "Minima Server crashed with exit code $?.  Respawning.." >&2
    sleep 3
done
