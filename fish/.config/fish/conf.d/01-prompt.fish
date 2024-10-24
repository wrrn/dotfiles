function humantime --argument-names ms --description "Turn milliseconds into a human-readable string"
    #     Copyright © Jorge Bucaran <https://jorgebucaran.com>

    # Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    # The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    # THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    set --query ms[1] || return

    set --local secs (math --scale=1 $ms/1000 % 60)
    set --local mins (math --scale=0 $ms/60000 % 60)
    set --local hours (math --scale=0 $ms/3600000)

    test $hours -gt 0 && set --local --append out $hours"h"
    test $mins -gt 0 && set --local --append out $mins"m"
    test $secs -gt 0 && set --local --append out $secs"s"

    set --query out && echo $out || echo $ms"ms"
end


if status is-interactive
    if type starship &>/dev/null
        starship init fish | source
        enable_transience

        function starship_transient_prompt_func
            echo -n "┏⭘ "
        end


        function write_box_start --on-event fish_preexec
            # This add a number of spaces before the empty string and then
            # replaces the spaces with ━.
            set curr_time "$(date +%T)"
            set box_prefix '┗'
            set box_suffix "$(printf ' %s ┓' "$curr_time")"

            set box_prefix_length (string length "$box_prefix")
            set box_suffix_length (string length "$box_suffix")

            set fill_count (math "$(tput cols) - $box_prefix_length - $box_suffix_length")

            set box_fill "$(string repeat -n "$fill_count" "━")"

            printf '%s%s%s\n\n' "$box_prefix" "$box_fill" "$box_suffix"
        end

        function write_box_end --on-event fish_postexec
            set box_prefix "┗"
            set box_suffix "$(printf " %s ┛" "$(humantime $CMD_DURATION)")"


            set box_prefix_length (string length "$box_prefix")
            set box_suffix_length (string length "$box_suffix")

            set fill_count (math "$(tput cols) - $box_prefix_length - $box_suffix_length")

            set box_fill "$(string repeat -n "$fill_count" "━")"


            printf '\n%s%s%s\n\n' "$box_prefix" "$box_fill" "$box_suffix"

        end

    end
end
