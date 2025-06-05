# docker aliases
abbr -a drma docker rm -f '(docker container ls -q -a)'

# git aliases
abbr -a g git
abbr -a gs git status
abbr -a glg git lg
abbr -a gco git checkout
abbr -a gcln git clean -d
abbr -a gr git restore
abbr -a grs git restore --staged
abbr -a ga git add
abbr -a gc git commit
abbr -a gcm git commit -m
abbr -a gd git diff
abbr -a gds git diff --staged
abbr -a gdu git diff-upstream
abbr -a gf git fetch
abbr -a gp git push
abbr -a gpl git pull
abbr -a gwip git wip
abbr -a grb git rebase
abbr -a grbc git rebase --continue
abbr -a gwrta git worktree add

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
abbr -a ls eza
abbr -a ll eza -l

# myip aliases
abbr -a myip dig +short myip.opendns.com @resolver1.opendns.com

# gcloud
abbr -a g gcloud
abbr -a gcloud-update-ssh-config "sed -i '/Host warren-dev/,/Host .*|\$/{ /HostName/{s/HostName .*/HostName '(gcloud compute instances describe warren-dev | yq .networkInterfaces[0].accessConfigs[0].natIP)'/; };}' ~/.ssh/config"

# worktree abbreviations
abbr -a ws --set-cursor "worktree switch % | source"

# directory generation
function workbench_dir
    set dir (string replace -r '^wb/' '' -- $argv)
    echo ~/shed/workbench/$dir
end
abbr --add workbench_dir_abbr --position anywhere --regex "wb/.*" --function workbench_dir
