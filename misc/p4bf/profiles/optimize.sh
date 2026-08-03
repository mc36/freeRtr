#!/bin/bash
set -e

api_url=$1
credentials=$2

./c.sh
[ -d rare-nix ] || git clone https://bitbucket.software.geant.org/scm/rare/rare-nix.git
rare_nix_commmit=$(cd rare-nix && git log -1 --format="%h")
sde_version=$(nix eval --raw --impure --expr '(with import ./rare-nix {}; bf-sde.version)')
nix-build -j auto -A bf-sde.envCommand rare-nix
if ! result/bin/sde-env-$sde_version --platform model --command './rp.sh;exit \$?'; then
    echo "Optimization failed for at least one profile"
    exit 1
fi
[ -z "$api_url" ] && exit 0

## We've been started via the optimizer webhook. Commit changes to a
## new branch and create a PR for it.
random=$(echo $RANDOM | md5sum | head -c 6)
branch=optimizer_${sde_version}_$random
git checkout -b $branch
git add $sde_version
if ! git commit -m "Add optimization for RARE-NIX $rare_nix_commmit (SDE $sde_version)"; then
    echo "No changes detected"
    exit 0
fi
git push origin $branch
curl -u "$credentials" -H "Content-Type:application/json" $api_url/pull-requests \
     -X POST --data '{"title": "Profile optimization for RARE-NIX version '$rare_nix_commmit'", "fromRef": { "id": "refs/heads/'$branch'" }, "toRef": { "id": "refs/heads/master" }} }'

exit 0
