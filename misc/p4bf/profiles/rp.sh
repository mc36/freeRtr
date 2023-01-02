#!/bin/bash
set -e
set -o pipefail

PATH=$PATH:$SDE_INSTALL/bin
ncpus=$(lscpu -J | jq -r '.lscpu[] | select(.field == "CPU(s):") | .data')
sde_version=$(p4c --version | awk '{print $2}')

cleanup () {
    git restore rare_profiles.p4
}

trap cleanup INT TERM EXIT

optimize_one () {
    local profile=$(basename --suffix .tmpl $1)
    echo "Starting optimization for $profile"
    java optimizer $1 $2 >${profile}.log && cp $profile.p4 $3
    echo -n "Optimization for $profile finished: "
    tail -1 ${profile}.log
    rm $2
    git restore $profile.p4
}

declare -A target_flags
target_flags[tofino]="--target tofino --arch=tna"
target_flags[tofino2]="--target tofino2 --arch=t2na"

optimize_target () {
    target=$1
    nprocs=0
    dir=$sde_version/$target
    echo "Optimizing for SDE $sde_version, target $target"
    test -e $dir && {
	echo "$sde_version/$target already exists"
	exit 1
    }
    mkdir -p $dir
    for fn in profile-*.tmpl ; do
	profile=$(echo $fn | sed -e 's/.*profile-\(.*\)\.tmpl/\1/' | tr [:lower:] [:upper:] | tr - _)
	file=optimizer_$profile.txt
	nix eval --extra-experimental-features nix-command --json --impure --expr \
	    '(with import rare-nix/. {}; flagsForProfile "'$profile'" "'$target'")' \
	    | jq -r '(.[] | join(" "))' \
	    | sed -e "s/^/${target_flags[$target]} /" >$file
	optimize_one $fn $file $dir &
	nprocs=$(($nprocs+1))
	if [ $nprocs -eq $ncpus ]; then
	    wait -n
	    nprocs=$(($nprocs-1))
	fi
    done
    wait
    echo "##undef _TABLE_SIZE_P4_" > $dir/rare_profiles.p4
    for fn in profile-*.p4 ; do
	echo "#include \"$fn\"" >> $dir/rare_profiles.p4
    done
}

echo "" >rare_profiles.p4
for target in tofino tofino2; do
    optimize_target $target
done
