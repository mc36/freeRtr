#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
sleep 5
ip link set ens4 up promisc on
ip link set ens5 up promisc on
ip link set ens6 up promisc on
ip link set ens7 up promisc on
ip link set ens8 up promisc on
sleep 1
ovs-vsctl init
ovs-vsctl del-br sw
sleep 1
ovs-vsctl add-br sw
ovs-vsctl set-controller sw tcp:10.10.10.227
ovs-vsctl add-port sw ens4
ovs-vsctl add-port sw ens5
ovs-vsctl add-port sw ens6
ovs-vsctl add-port sw ens7
ovs-vsctl add-port sw ens8
sleep 1
/home/mc36/dump.sh
