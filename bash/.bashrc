export BASH_CONF="bashrc"
# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Only show three directories
PROMPT_DIRTRIM=3

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

export LC=en_US.UTF-8
export LANGUAGE=en_US.UTF
export LANG="en_US.UTF-8"

if tput Co > /dev/null 2>&1; then
    test "`tput Co`" -gt 2 && color_prompt=yes
elif tput colors > /dev/null 2>&1; then
    test "`tput colors`" -gt 2 && color_prompt=yes
fi

if [ "$color_prompt" = yes ]; then
    PS1='\[\033[01;32m\]\u@\H\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
else
    PS1='\u@\H:\W\$ '
fi

export PS1
unset color_prompt

# some more ls aliases
alias ls='ls --color=always'
alias sudo='sudo '
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CFG'
alias less='less -r' 
alias emacs='emacs -nw'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -z ${PROFILE_CALLED:+x} ]; then
    source ~/.profile
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -f ~/.bashrc.local ]; then
    . ~/.bashrc.local
fi
if [ -f $HOME/.local/bin/aws_bash_completer ]; then
    source $HOME/.local/bin/aws_bash_completer
fi

