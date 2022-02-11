#!/bin/bash
set -e
ncpus=$(lscpu -J | jq -r '.lscpu[] | select(.field == "CPU(s):") | .data')

optimize_one () {
    local profile=$(basename --suffix .tmpl $1)
    echo "Starting optimization for $profile"
    java optimizer $1 >${profile}.log
    echo -n "Optimization for $profile finished: "
    tail -1 ${profile}.log
}

nprocs=0
for fn in *.tmpl ; do
    optimize_one $fn &
    nprocs=$(($nprocs+1))
    if [ $nprocs -eq $ncpus ]; then
	wait -n
	nprocs=$(($nprocs-1))
    fi
done
wait
echo "##undef _TABLE_SIZE_P4_" > rare_profiles.p4
for fn in *.p4 ; do
    [ $fn == rare_profiles.p4 ] && continue
    echo "#include \"$fn\"" >> rare_profiles.p4
done
