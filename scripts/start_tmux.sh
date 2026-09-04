session=clj-money

tmux new-session -d -s $session

ACCENT_COLOR="#2b7a2d"
tmux set -t $session status-style "bg=$ACCENT_COLOR fg=#CCCCCC"

# REPL window
tmux rename-window -t 0 'repl'
sleep 0.5
tmux send-keys 'clear' C-m 'lein repl' C-m

tmux split-window -v
while [ ! -f .nrepl-port ]; do sleep 1; done
sleep 0.5
tmux send-keys 'lein fig:build' C-m

# Split the top (repl) pane to run Caddy, unless it's already running
tmux split-window -v -t $session:0.0
sleep 0.5
tmux send-keys 'pgrep -x caddy >/dev/null || mise run caddy' C-m

# Code window
tmux new-window -t $session:1 -n $session
sleep 0.5
tmux send-keys 'nvim' C-m
tmux split-window -h
sleep 0.5
tmux send-keys 'git status' C-m
tmux split-window -v
sleep 0.5
tmux send-keys 'claude' C-m

# Database window
tmux new-window -t $session:2 -n 'database'
tmux send-keys 'psql' C-m


# Log window
tmux new-window -t $session:3 -n 'logs'
sleep 0.5
tmux send-keys 'tail -f log/development.log | grep -e ERROR -e WARN -e dbk' C-m
tmux split-window -v
sleep 0.5
tmux send-keys 'tail -f log/development.log' C-m

# pane-active-border-style is a window option, not a session option, so it
# must be (re)applied to every window rather than set once at session start.
for w in $(tmux list-windows -t $session -F '#{window_index}'); do
  tmux set -t $session:$w pane-active-border-style "fg=$ACCENT_COLOR"
done

tmux attach -t $session:1
