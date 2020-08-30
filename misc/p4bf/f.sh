#!/bin/sh
export SDE=/usr/share/bf_switchd
export SDE_INSTALL=/usr/share/bf_switchd/install
###sleep 15
cd /home/mc36/rare/bfrt_python
while (true); do
  ./bf_forwarder.py --freerouter-address 10.10.10.227
  done
