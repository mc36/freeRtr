#!/bin/sh
cd /home/mc36/rare/p4src
export SDE=/home/mc36/bf-sde-9.13.4
export SDE_INSTALL=$SDE/install
while (true); do
  $SDE/run_switchd.sh --arch tf1 -p bf_router
  done
