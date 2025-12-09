#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on mtu 2048
ip link set ens5 up promisc on mtu 2048
ip link set ens6 up promisc on mtu 2048
ip link set ens7 up promisc on mtu 2048
ip link set ens8 up promisc on mtu 2048
while (true); do
  /home/mc36/p4xsk.bin 10.10.10.227 9080 0 drv ens4 ens5 ens6 ens7 ens8
  done
