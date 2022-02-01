#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on mtu 8192
ip link set ens5 up promisc on mtu 8192
ip link set ens6 up promisc on mtu 8192
ip link set ens7 up promisc on mtu 8192
ip link set ens8 up promisc on mtu 8192
while (true); do
  /home/mc36/p4xdp_user.bin 10.10.10.227 9080 0 ens4 ens5 ens6 ens7 ens8
  done
