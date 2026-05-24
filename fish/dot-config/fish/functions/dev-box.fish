function dev-box
    switch $argv[1]
        case create
            _dev-box-create
        case start up
            _dev-box-start
        case stop down
            gcloud compute instances stop warren-dev
        case delete
            gcloud compute instances delete warren-dev
    end
end

function _dev-box-start
    if not _dev-box-is-up
        _dev-box-create
    end

    gcloud \
        --project pryon-mainline-e2e-1 \
        compute \
        instances start \
        warren-dev

    set -l ip (gcloud compute instances describe warren-dev | yq .networkInterfaces[0].accessConfigs[0].natIP)

    sed \
        -i \
        '/Host warren-dev/,/Host .*|\$/{ /HostName/{s/HostName .*/HostName '$ip'/; };}' \
        ~/.ssh/config

    ssh-keyscan $ip >>~/.ssh/known_hosts
end

function _dev-box-is-up
    set -l uri (
    gcloud compute instances list \
        --filter 'name=(warren-dev)' \
        --quiet \
        --uri)

    string length -q $uri
end

function _dev-box-create
    gcloud compute instances create warren-dev \
        --project=pryon-mainline-e2e-1 \
        --zone=us-central1-a \
        --machine-type=n2d-standard-32 \
        --network-interface=network-tier=PREMIUM,stack-type=IPV4_ONLY,subnet=warren-dev-us-central1 \
        --no-restart-on-failure \
        --maintenance-policy=TERMINATE \
        --provisioning-model=SPOT \
        --instance-termination-action=STOP \
        --service-account=464539289176-compute@developer.gserviceaccount.com \
        --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/trace.append \
        --tags=allow-22,warren-dev-allow-mosh \
        --create-disk=auto-delete=yes,boot=yes,device-name=instance-20240828-142137,disk-resource-policy=projects/pryon-mainline-e2e-1/regions/us-central1/resourcePolicies/us-central1-opensearch-daily,image=projects/debian-cloud/global/images/debian-12-bookworm-v20240815,mode=rw,size=100,type=pd-balanced \
        --no-shielded-secure-boot \
        --shielded-vtpm \
        --shielded-integrity-monitoring \
        --labels=goog-ec-src=vm_add-gcloud \
        --reservation-affinity=any
end



complete \
    --command dev-box \
    --arguments 'create up start down stop delete' \
    --exclusive
