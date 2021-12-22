#!/bin/sh
cd /home/mc36/rare/p4src
export SDE=/home/mc36/bf-sde-9.7.0
export SDE_INSTALL=$SDE/install
while (true); do
  $SDE/run_switchd.sh -p bf_router
  done
