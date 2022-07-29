# docker aliases
abbr -a drma docker rm -f (docker container ls -q -a)

# git aliases
abbr -a g git
abbr -a gs git status
abbr -a gco git checkout
abbr -a gr git rebase
abbr -a ga git add
abbr -a gc git commit

# kubernetes aliases
abbr -a k kubectl
abbr -a kh kubectl help
abbr -a ke exec -ti
abbr -a kaa apply -R -f .
abbr -a klf logs -f
abbr -a kgy get -o yaml

# make
abbr -a m make

# tmux
abbr -a tn tmux new -s


# global aliases
abbr -a g grep
abbr -a b bat
abbr -a by bat -lyaml
abbr -a bj bat -ljson

# ls
abbr -a ls exa
abbr -a ll exa -l

# myip aliases
abbr -a myip dig +short myip.opendns.com @resolver1.opendns.com


