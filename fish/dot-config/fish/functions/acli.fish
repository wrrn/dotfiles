function acli --wraps=acli
    if test "$argv[1]" = jira
        and test "$argv[2]" = workitem
        and test "$argv[3]" = mine
        command acli jira workitem search \
            --jql 'assignee IN (712020:c0f68d49-3cbc-4c54-a130-eb9a31f551c5) and status NOT IN (Done)' \
            $argv
        return
    end
    command acli $argv
end
