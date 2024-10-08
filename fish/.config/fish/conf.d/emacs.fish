set -x EMACS_VTERM_PATH ~/.emacs.d/straight/build/vterm

if [ "$INSIDE_EMACS" = vterm ] && [ -n $EMACS_VTERM_PATH ] && [ -f {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish ]
    source {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish
end

if [ "$INSIDE_EMACS" = vterm ]
    set -x EDITOR emacsclient
else if string match -q -- '*kitty' $TERM
    set -x EDITOR {$HOME}/.local/bin/kitty.emacs
else
    set -x EDITOR 'emacsclient -nw'
end
