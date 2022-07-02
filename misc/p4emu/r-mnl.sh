#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
echo 0 > /proc/sys/net/ipv6/conf/ens5nrockerp1/disable_ipv6
echo 0 > /proc/sys/net/ipv6/conf/ens5nrockerp2/disable_ipv6
echo 0 > /proc/sys/net/ipv6/conf/ens5nrockerp3/disable_ipv6
echo 0 > /proc/sys/net/ipv6/conf/ens5nrockerp4/disable_ipv6
ip link set ens4 up promisc on mtu 2048
ip link set ens5nrockerp1 up promisc on mtu 2048
ip link set ens5nrockerp2 up promisc on mtu 2048
ip link set ens5nrockerp3 up promisc on mtu 2048
ip link set ens5nrockerp4 up promisc on mtu 2048
while (true); do
  /home/mc36/p4mnl_user.bin 10.10.10.227 9080 0 ens4 ens5nrockerp1 ens5nrockerp2 ens5nrockerp3 ens5nrockerp4
  done
