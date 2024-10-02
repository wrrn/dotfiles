#!/bin/bash

KITTEN="kitten @ --to ${KITTY_LISTEN_ON}"

${KITTEN} set-user-vars --match id:${KITTY_WINDOW_ID} in_editor=MQo
${KITTEN} push_keyboard_mode emacs --match id:${KITTY_WINDOW_ID} in_editor=MQo
/opt/homebrew/bin/emacsclient -nw "$@"
${KITTEN} pop_keyboard_mode --match id:${KITTY_WINDOW_ID} in_editor=MQo
${KITTEN} set-user-vars --match id:${KITTY_WINDOW_ID} in_editor
