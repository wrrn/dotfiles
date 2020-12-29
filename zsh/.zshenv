export WORK=$HOME/work
export GOPATH=$WORK/go

PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
PATH="/usr/local/bin:$PATH"
PATH="$GOPATH/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH

export EDITOR=emacsclient
