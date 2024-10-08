if status is-interactive
    if type starship &>/dev/null
        function starship_transient_rprompt_func
            starship module time
        end
        starship init fish | source
        enable_transience
        starship init fish | source

    end
end
