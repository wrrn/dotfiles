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
source /Users/warren.harper/.nix-profile/etc/profile.d/nix.sh
pathmunge "$HOME/.cargo/bin"
pathmunge "/usr/local/opt/coreutils/libexec/gnubin"
pathmunge "/usr/local/opt/gnu-sed/libexec/gnubin"
pathmunge "/usr/local/bin"
pathmunge "$GOPATH/bin"
export PATH
