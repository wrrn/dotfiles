if status is-interactive
    if type starship &>/dev/null
        starship init fish | source
    end

    if [ "$INSIDE_EMACS" = 'vterm' ] &&  [ -n $EMACS_VTERM_PATH ] &&  [ -f {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish ]
        source {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish
    end
end
