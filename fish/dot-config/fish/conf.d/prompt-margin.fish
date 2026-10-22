function _prompt_margin --on-event fish_prompt
    set -l n (math "floor($LINES / 3)")
    printf '\n%.0s' (seq $n)
    printf '\e[%dA' $n
end
