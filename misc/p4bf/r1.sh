#!/bin/sh
export SDE=/usr/share/bf_switchd
export SDE_INSTALL=/usr/share/bf_switchd/install
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set enp0s4 up promisc on
ip link set enp0s5 up promisc on
ip link set enp0s6 up promisc on
ip link set enp0s7 up promisc on
ip link set enp0s8 up promisc on
cd /home/mc36/rare/p4src
while (true); do
  $SDE/run_tofino_model.sh -p bf_router -f /home/mc36/ports.json -q
  done
