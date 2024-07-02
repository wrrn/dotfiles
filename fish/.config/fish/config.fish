set -x GOPATH $HOME/.go
fish_add_path --prepend \
    $HOME/.local/bin \
    $HOME/bin \
    $GOPATH/bin \
    $BREW_PREFIX/bin \
    $BREW_PREFIX/opt/gnu-sed/libexec/gnubin \
    $BREW_PREFIX/opt/coreutils/libexec/gnubin \
    $BREW_PREFIX/opt/gnu-tar/libexec/gnubin \
    $HOME/.cargo/bin \
    /usr/local/bin

# Add
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


set -x USE_GKE_GCLOUD_AUTH_PLUGIN True
set -x EDITOR emacsclient

set fish_greeting

if status is-interactive
    # Commands to run in interactive sessions can go here

    if type zoxide &>/dev/null
        zoxide init fish | source
    end

    if type direnv &>/dev/null
        direnv hook fish | source
    end

    source /opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc
    source "$(brew --prefix)/share/google-cloud-sdk/path.fish.inc"

    if type kind &>/dev/null && [ ! -f ~/.config/fish/completions/kind.fish ] || [ ! -f ~/.kube/.kind.version ] || [ (kind version) != (cat ~/.kube/.kind.version) ]
        kind completion fish >~/.config/fish/completions/kind.fish
    end

    if [ "$INSIDE_EMACS" = vterm ]
        set -x EDITOR emacsclient
    else
        set -x EDITOR 'emacsclient -nw'
    end



    ## Notes
    # kubectl completions come from the fisher plugin
    # fzf comes from fisher plugin
end

# source /nix/var/nix/profiles/default/etc/profile.d/nix.fish
