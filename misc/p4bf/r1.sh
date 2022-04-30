#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on
ip link set ens5 up promisc on
ip link set ens6 up promisc on
ip link set ens7 up promisc on
ip link set ens8 up promisc on
cd /home/mc36/rare/p4src
export SDE=/home/mc36/bf-sde-9.9.0
export SDE_INSTALL=$SDE/install
while (true); do
  $SDE/run_tofino_model.sh -p bf_router -f /home/mc36/ports.json -q
  done
