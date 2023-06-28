# docker aliases
abbr -a drma docker rm -f '(docker container ls -q -a)'

# git aliases
abbr -a g git
abbr -a gs git status
abbr -a gco git checkout
abbr -a gr git restore
abbr -a grs git restore --staged
abbr -a ga git add
abbr -a gc git commit
abbr -a gcm git commit -m
abbr -a gd git diff
abbr -a gds git diff --staged
abbr -a gf git fetch
abbr -a gp git push
abbr -a gpl git pull
abbr -a gwip git wip
abbr -a grb git rebase
abbr -a grbc git rebase --continue

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
abbr -a yaml bat -lyaml
abbr -a json bat -ljson
abbr -a cat bat
abbr -a dog bat

# ls
abbr -a ls exa
abbr -a ll exa -l

# myip aliases
abbr -a myip dig +short myip.opendns.com @resolver1.opendns.com
