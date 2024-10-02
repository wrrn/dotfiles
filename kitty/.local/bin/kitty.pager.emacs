#!/bin/bash

# Call emacsclient as a pager.
# First, read in the content of stdin into a temporary file
line="$1"
column="$2"

mktemp=mktemp
if type /opt/homebrew/bin/brew > /dev/null 2>&1; then
    mktemp=$(/opt/homebrew/bin/brew --prefix)/opt/coreutils/libexec/gnubin/mktemp
fi

t=$(${mktemp} /tmp/emacsclient.XXXXXX.ktty) || exit 1

echo "Reading into emacs..."
cat - >> $t

/opt/homebrew/bin/emacsclient -nw +${line}:${column} "$t"
rm -f $t
