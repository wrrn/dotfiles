set -x BREW_PREFIX /opt/homebrew
if not contains "$BREW_PREFIX/bin" $PATH
    set -gx --prepend PATH $BREW_PREFIX/bin
end
set -x XDG_DATA_DIRS /opt/homebrew/share/ $XDG_DATA_DIRS
