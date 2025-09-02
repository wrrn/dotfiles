if not command -q acli
    ## ACLI is not installed
    exit 0
end

acli completion fish | source

function __acli_complete_my_tickets
    acli jira workitem search \
        --jql '(sprint IN futureSprints() OR sprint IN openSprints()) AND assignee = currentUser()' \
        --json \
        | jq '.[] | "\(.key)\t\(.fields.summary):\(.fields.status.name)"' -r
end

function __acli_is_subcommand
    set -l subcommand "$argv"
    set -l cmd (commandline --current-process)
    string match -q "acli $subcommand *" "$cmd"
end

complete --command acli \
    --short-option k \
    --long-option key \
    --no-files \
    --require-parameter \
    --condition '__acli_is_subcommand jira workitem' \
    --arguments '(__acli_complete_my_tickets)' \
    --description "Jira issue key"

complete --command acli \
    --short-option s \
    --long-option status \
    --no-files \
    --require-parameter \
    --condition '__acli_is_subcommand jira workitem' \
    --arguments 'Development Blocked "Code Change Needed" "In Review" "Done" "Won\'t Do"' \
    --description "Jira status"
