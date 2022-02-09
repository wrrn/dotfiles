dont-slack() {
    caffeinate -d -i -m -u -w $(pgrep -xo Slack) &
    disown
}

pathmunge () {
    PATH=${PATH/":${1}:"/:}
    PATH=${PATH/":${1}"/}
    PATH=${PATH/"${1}:"/}
    
    if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
    else
        PATH=$1:$PATH
    fi
}

if [ -f /Users/warren.harper/.nix-profile/etc/profile.d/nix.sh ]; then
    source /Users/warren.harper/.nix-profile/etc/profile.d/nix.sh
fi

pathmunge "$HOME/.cargo/bin"
pathmunge "/usr/local/opt/coreutils/libexec/gnubin"
pathmunge "/usr/local/opt/gnu-sed/libexec/gnubin"
pathmunge "/usr/local/bin"
pathmunge "$GOPATH/bin"
export PATH

export EDITOR=emacsclient

if [ -f $HOME/.zprofile.custom ]; then 
   source $HOME/.zprofile.custom
fi
eval "$(pyenv init --path)"
