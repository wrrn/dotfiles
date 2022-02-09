set -x GOPATH $HOME/work/go
set -x PATH \
    $HOME/.local/bin \
    $HOME/bin \
    $GOPATH/bin \
    /usr/local/bin \
    /usr/local/opt/gnu-sed/libexec/gnubin \
    /usr/local/opt/coreutils/libexec/gnubin \
    $HOME/.cargo/bin \
    $PATH

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

    if type kind &>/dev/null && \
        [ ! -f ~/.config/fish/completions/kind.fish ] || \
        [ ! -f ~/.kube/.kind.version ] || \
        [ (kind version) != (cat ~/.kube/.kind.version) ]
             kind completion fish > ~/.config/fish/completions/kind.fish
    end

    source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc
    bass source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc

    source (brew --prefix asdf)/libexec/asdf.fish
    . ~/.asdf/plugins/java/set-java-home.fish

    ## Notes
    # kubectl completions come from the fisher plugin
    # fzf comes from fisher plugin
end
