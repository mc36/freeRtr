#!/bin/bash
set -e

./c.sh
[ -d rare-nix ] || git clone https://bitbucket.software.geant.org/scm/rare/rare-nix.git
nix-build -j auto -A bf-sde.envCommand rare-nix
result/bin/sde-env-* --platform model --command "./rp.sh;exit"
