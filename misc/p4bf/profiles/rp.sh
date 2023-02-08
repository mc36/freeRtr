#!/bin/bash
set -e
set -o pipefail

PATH=$PATH:$SDE_INSTALL/bin
ncpus=$(lscpu -J | jq -r '.lscpu[] | select(.field == "CPU(s):") | .data')
sde_version=$(p4c --version | awk '{print $2}')
rc=0

cleanup () {
    git restore rare_profiles.p4
}

trap cleanup INT TERM EXIT

optimize_one () {
    local profile=$(basename --suffix .tmpl $1) rc=0
    echo "Starting optimization for $profile"
    if java optimizer $1 $2 >${profile}.log; then
        cp $profile.p4 $3
    else
        rc=1
    fi
    echo -n "Optimization for $profile finished: "
    tail -1 ${profile}.log
    rm $2
    git restore $profile.p4
    return $rc
}

declare -A target_flags
target_flags[tofino]="--target tofino --arch=tna"
target_flags[tofino2]="--target tofino2 --arch=t2na"

optimize_target () {
    target=$1
    nprocs=0
    dir=$sde_version/$target
    echo "Optimizing for SDE $sde_version, target $target"
    mkdir -p $dir
    for fn in profile-*.tmpl; do
        profile=$(echo $fn | sed -e 's/.*profile-\(.*\)\.tmpl/\1/' | tr [:lower:] [:upper:] | tr - _)
        file=optimizer_$profile.txt
        nix eval --extra-experimental-features nix-command --json --impure --expr \
            '(with import rare-nix/. {}; flagsForProfile "'$profile'" "'$target'")' \
            | jq -r '(.[] | join(" "))' \
            | sed -e "s/^/${target_flags[$target]} /" >$file
        optimize_one $fn $file $dir &
        nprocs=$(($nprocs+1))
        if [ $nprocs -eq $ncpus ]; then
            wait -n || rc=1
            nprocs=$(($nprocs-1))
        fi
    done
    while [ $nprocs -gt 0 ]; do
        wait -n || rc=1
        nprocs=$(($nprocs-1))
    done
    echo "##undef _TABLE_SIZE_P4_" > $dir/rare_profiles.p4
    for fn in profile-*.p4 ; do
        echo "#include \"$fn\"" >> $dir/rare_profiles.p4
    done
}

echo "" >rare_profiles.p4
for target in tofino tofino2; do
    optimize_target $target
done
exit $rc
