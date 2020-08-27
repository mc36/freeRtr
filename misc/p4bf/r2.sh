#!/bin/sh
export SDE=/usr/share/bf_switchd
export SDE_INSTALL=/usr/share/bf_switchd/install
cd /home/mc36/rare/p4src
while (true); do
  $SDE/run_switchd.sh -p bf_router
  done
