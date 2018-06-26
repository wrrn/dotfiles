# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
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

export EDITOR="emacsclient"

pathmunge /usr/local/bin

if [ -r $HOME/.profile.local ]; then
    source $HOME/.profile.local
fi

if command -v brew > /dev/null 2>&1; then
    BREW_PREFIX="$(brew --prefix)"
    pathmunge "${BREW_PREFIX}/opt/coreutils/libexec/gnubin"
    MANPATH="${BREW_PREFIX}/opt/coreutils/libexec/gnuman:${MANPATH}"
    source ${BREW_PREFIX}/etc/bash_completion
fi

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi


if [ -f "/etc/bash_completion" ]; then
    source /etc/bash_completion
fi

if [ -r $HOME/.local/etc/bash_completion ]; then
    source $HOME/.local/etc/bash_completion
fi

PROFILE_CALLED=true
GOPATH=$HOME/shed/go
pathmunge $GOPATH/bin
TERMINAL=alacritty
pathmunge "$HOME/bin"

pathmunge /usr/local/go/bin after

export GOPATH
export PATH

if [ -r $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi

if [ -d $HOME/.local/bin ]; then
    pathmunge $HOME/.local/bin
fi

pathmunge "$HOME/.cargo/bin"
export TERMINAL

unset -f pathmunge
