# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [ -d "$GHC_DOT_APP" ]; then
    PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

export EDITOR="emacsclient"


PATH="${PATH}:$HOME/.rvm/bin:${HOME}/bin" # Add RVM to PATH for scripting
export PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"

MANPATH="/usr/local/opt/coreutils/libexec/gnuman:${MANPATH}"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    source $HOME/.nix-profile/etc/profile.d/nix.sh
fi


PROFILE_CALLED=true
GOPATH=${HOME}/workshop/go
PATH=$PATH:$GOPATH/bin

if [ -r $HOME/.profile.local ]; then
    source $HOME/.profile.local
fi


export GOPATH
export PATH

if [ -r $HOME/.cargo/env ]; then
    source $HOME/.cargo/env
fi

export PATH="$HOME/.cargo/bin:$PATH"
