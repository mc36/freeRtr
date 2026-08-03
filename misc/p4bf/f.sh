#!/bin/sh
###sleep 15
cd /home/mc36/rare/bfrt_python
export SDE=/home/mc36/bf-sde-9.13.4
export SDE_INSTALL=$SDE/install
export PROTOCOL_BUFFERS_PYTHON_IMPLEMENTATION=python
while (true); do
  python3 bf_forwarder.py --freerouter-address 10.10.10.227
  done
