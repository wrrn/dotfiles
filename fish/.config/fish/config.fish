set -x GOPATH $HOME/work/go
set -x PATH \
    $HOME/.local/bin \
    $HOME/bin \
    $GOPATH/bin \
    $BREW_PREFIX/bin \
    /usr/local/bin \
    $BREW_PREFIX/opt/gnu-sed/libexec/gnubin \
    $BREW_PREFIX/opt/coreutils/libexec/gnubin \
    $BREW_PREFIX/opt/gnu-tar/libexec/gnubin \
    $HOME/.cargo/bin \
    $PATH
set -x USE_GKE_GCLOUD_AUTH_PLUGIN True
set -x EDITOR emacsclient -nw

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

    if type kind &>/dev/null && [ ! -f ~/.config/fish/completions/kind.fish ] || [ ! -f ~/.kube/.kind.version ] || [ (kind version) != (cat ~/.kube/.kind.version) ]
        kind completion fish >~/.config/fish/completions/kind.fish
    end

    source (brew --prefix asdf)/libexec/asdf.fish
    . ~/.asdf/plugins/java/set-java-home.fish

    if [ "$INSIDE_EMACS" = vterm ]
        set -x EDITOR emacsclient
    end
    ## Notes
    # kubectl completions come from the fisher plugin
    # fzf comes from fisher plugin
end

# source /nix/var/nix/profiles/default/etc/profile.d/nix.fish
