## Add the ability to track the directory in vterm
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

## If we are running vterm in emacs then this will clear everything for us.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

## Autoload our zsh completions that were installed via brew.
if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
    
    autoload -Uz compinit
    compinit
fi

if type kubectl &>/dev/null; then
    source <(kubectl completion zsh)
fi

if type kind &>/dev/null; then
    kind completion zsh > "${fpath[1]}/_kind"
fi

setopt PROMPT_SUBST
if type starship &>/dev/null; then
    eval "$(starship init zsh)"
fi

setopt auto_cd

source $HOME/.aliases

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
