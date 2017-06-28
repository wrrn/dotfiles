# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/

export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [ -d "$GHC_DOT_APP" ]; then
    PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

export EDITOR="emacsclient"


PATH="${PATH}:$HOME/.rvm/bin:${HOME}/bin" # Add RVM to PATH for scripting
if [ -d /usr/local/opt/coreutils/libexec/gnubin/ ]; then
    PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
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
PATH=$PATH:$GOPATH/bin
TERMINAL=alacritty

if [ -r $HOME/.profile.local ]; then
    source $HOME/.profile.local
fi


export GOPATH
export PATH

if [ -r $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi

if [ -d $HOME/.local/bin ]; then
    PATH=$PATH:$HOME/.local/bin
fi

export PATH="$HOME/.cargo/bin:$PATH"
export TERMINAL
