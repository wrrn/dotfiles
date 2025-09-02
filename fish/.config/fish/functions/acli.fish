function acli-my-tickets --wraps=acli
    acli jira workitem search \
        --jql '(sprint IN futureSprints() OR sprint IN openSprints()) AND assignee = currentUser()' $argv
end
