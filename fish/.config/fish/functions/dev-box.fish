function dev-box
    switch $argv[1]
        case start up
            gcloud --project pryon-mainline-e2e-1 \
                compute \
                instances start \
                warren-dev

            sed -i '/Host warren-dev/,/Host .*|\$/{ /HostName/{s/HostName .*/HostName '(gcloud compute instances describe warren-dev | yq .networkInterfaces[0].accessConfigs[0].natIP)'/; };}' ~/.ssh/config

        case stop down
            gcloud compute instances stop warren-dev
    end
end

complete --command dev-box \
    --arguments 'up start down stop' \
    --exclusive
