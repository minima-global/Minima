#!/bin/sh

# var for session name (to avoid repeated occurences)
sn=abc

tmux new-session -s "$sn" -d -x "$(tput cols)" -y "$(tput lines)"

# Set the default cwd for new windows (optional, otherwise defaults to session cwd)
#tmux set-option default-path /

tmux rename-window "PUBLIC"

#FIRST create the pane windows.. PUBLIC
tmux selectp -t 0    # select the first (0) pane
tmux splitw -h -p 66  # split it into two halves
tmux selectp -t 1
tmux splitw -h -p 50 # split it into two halves

tmux selectp -t 0
tmux splitw -v -p 50
tmux splitw -v -p 50
tmux selectp -t 0
tmux splitw -v -p 50

tmux selectp -t 4
tmux splitw -v -p 50
tmux splitw -v -p 50
tmux selectp -t 4
tmux splitw -v -p 50

tmux selectp -t 8
tmux splitw -v -p 50
tmux splitw -v -p 50
tmux selectp -t 8
tmux splitw -v -p 50

#PUBLIC
tmux selectp -t 0
tmux send-keys "gcloud compute ssh minima-public-1-vm  -- htop" Enter
tmux selectp -t 1
tmux send-keys "gcloud compute ssh minima-public-2-vm  -- htop" Enter
tmux selectp -t 2
tmux send-keys "gcloud compute ssh minima-public-3-vm  -- htop" Enter
tmux selectp -t 3
tmux send-keys "gcloud compute ssh minima-public-4-vm  -- htop" Enter
tmux selectp -t 4
tmux send-keys "gcloud compute ssh minima-public-5-vm  -- htop" Enter
tmux selectp -t 5
tmux send-keys "gcloud compute ssh minima-public-6-vm  -- htop" Enter

#MINERS
tmux selectp -t 6
tmux send-keys "gcloud compute ssh minima-miner-1-vm --zone=europe-west2-a -- htop" Enter
tmux selectp -t 7
tmux send-keys "gcloud compute ssh minima-miner-2-vm --zone=europe-west2-a -- htop" Enter
tmux selectp -t 8
tmux send-keys "gcloud compute ssh minima-miner-3-vm --zone=europe-west2-a -- htop" Enter
tmux selectp -t 9
tmux send-keys "gcloud compute ssh minima-miner-4-vm --zone=europe-west2-a -- htop" Enter

#PRIVATE
tmux selectp -t 10
tmux send-keys "gcloud compute ssh minima-private-1-vm  -- htop" Enter
tmux selectp -t 11
tmux send-keys "gcloud compute ssh minima-private-2-vm  -- htop" Enter

#Attach to the session
tmux -2 attach-session -d 
 