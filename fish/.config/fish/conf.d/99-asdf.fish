set ASDF_DIR (asdf info | grep ASDF_DIR | string replace "ASDF_DIR=" "")

source $ASDF_DIR/asdf.fish
