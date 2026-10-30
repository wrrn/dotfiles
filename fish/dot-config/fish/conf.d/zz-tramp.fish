# TRAMP (and other dumb terminals) need a plain prompt they can recognize.
if test "$TERM" = dumb
    function fish_prompt
        echo -n '$ '
    end
    function fish_right_prompt
    end
    function fish_mode_prompt
    end
    functions --erase _prompt_margin
end
