#!/bin/sh
###sleep 15
cd /home/mc36/rare/bfrt_python
export SDE=/home/mc36/bf-sde-9.8.0
export SDE_INSTALL=$SDE/install
while (true); do
  ./bf_forwarder.py --freerouter-address 10.10.10.227
  done
