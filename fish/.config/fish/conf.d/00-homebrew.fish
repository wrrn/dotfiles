fish_add_path --prepend --path /usr/local/bin

if ! string match -q Darwin "$(uname)"
    return 0
end

set -x BREW_PREFIX /opt/homebrew

set -x XDG_DATA_DIRS /opt/homebrew/$BREW_PREFIX $XDG_DATA_DIRS

if not contains "$BREW_PREFIX/bin" $PATH
    set -gx --prepend PATH $BREW_PREFIX/bin
end

fish_add_path --prepend --path \
    $BREW_PREFIX/bin \
    $BREW_PREFIX/opt/gnu-sed/libexec/gnubin \
    $BREW_PREFIX/opt/coreutils/libexec/gnubin \
    $BREW_PREFIX/opt/gnu-tar/libexec/gnubin
