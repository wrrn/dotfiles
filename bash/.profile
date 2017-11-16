# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
pathmunge () {
    case ":${PATH}:" in
        *:"$1":*)
            ;;
        *)
            if [ "$2" = "after" ] ; then
                PATH=$PATH:$1
            else
                PATH=$1:$PATH
            fi
    esac
}

export EDITOR="emacsclient"

pathmunge /usr/local/bin



if [ -d /usr/local/opt/coreutils/libexec/gnubin/ ]; then
    pathmunge "/usr/local/opt/coreutils/libexec/gnubin"
fi

if [ -d /usr/local/opt/coreutils/libexec/gnuman ]; then
    MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}"
fi


if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi


if [ -f "$(brew --prefix || echo "")/etc/bash_completion" ]; then
    source $(brew --prefix)/etc/bash_completion
fi

if [ -r $HOME/.local/etc/bash_completion ]; then
    source $HOME/.local/etc/bash_completion
fi

PROFILE_CALLED=true
GOPATH=$HOME/go
pathmunge $GOPATH/bin
TERMINAL=alacritty
pathmunge "$HOME/bin"
if [ -r $HOME/.profile.local ]; then
    source $HOME/.profile.local
fi
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
