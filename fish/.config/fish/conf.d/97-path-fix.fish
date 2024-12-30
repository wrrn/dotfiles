# Apple has a hard time with these paths. It keeps putting them at the beginning.
fish_add_path --append --move --path \
    /bin \
    /usr/local/bin \
    /usr/bin \
    /user/sbin \
    /sbin

# For some reason /run/wrapper/bin is being falling after
# /run/current/system/sw/bin. This causes issues when running sudo. Adding it to the path fixes it.
fish_add_path --move --path /run/wrappers/bin
