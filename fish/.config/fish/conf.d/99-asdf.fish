source (brew --prefix asdf)/libexec/asdf.fish
set -l _asdf_bin "$ASDF_DIR/bin"
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end
fish_add_path --prepend \
    $_asdf_bin \
    $_asdf_shims
set --erase _asdf_bin
set --erase _asdf_shims
