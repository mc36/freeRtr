#!/bin/bash
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on
ip link set ens5 up promisc on
ip link set ens6 up promisc on
ip link set ens7 up promisc on
ip link set ens8 up promisc on
sleep 5
ovs-vsctl init
ovs-vsctl del-br sw
ovs-vsctl add-br sw
ovs-vsctl set-controller sw tcp:10.10.10.227:6653
ovs-vsctl add-port sw ens4
ovs-vsctl add-port sw ens5
ovs-vsctl add-port sw ens6
ovs-vsctl add-port sw ens7
ovs-vsctl add-port sw ens8
sleep 1
/bin/busybox start-stop-daemon -S -b -x /home/mc36/dumpd.sh
/home/mc36/dump.sh
