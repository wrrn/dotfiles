function add_path
    set -l _dirs
    for dir in $argv
        if not contains $dir $PATH
            set -a _dirs $dir
        end
    end

    set --prepend -gx PATH $_dirs
end
