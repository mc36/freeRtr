#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on
ip link set ens5 up promisc on
ip link set ens6 up promisc on
ip link set ens7 up promisc on
ip link set ens8 up promisc on
cd /home/mc36
while (true); do
  simple_switch_grpc -i 64@ens4 -i 1@ens5 -i 2@ens6 -i 3@ens7 -i 4@ens8 --thrift-port 9090 router.json
# --log-console -L debug --dump-packet-data 256
  done
