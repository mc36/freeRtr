#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on mtu 8192
ip link set ens5 up promisc on mtu 8192
ip link set ens6 up promisc on mtu 8192
ip link set ens7 up promisc on mtu 8192
ip link set ens8 up promisc on mtu 8192
start-stop-daemon -S -b -n ens4 -x /home/mc36/pcapInt.bin ens4 10012 127.0.0.1 10011 127.0.0.1
start-stop-daemon -S -b -n ens5 -x /home/mc36/pcapInt.bin ens5 10022 127.0.0.1 10021 127.0.0.1
start-stop-daemon -S -b -n ens6 -x /home/mc36/pcapInt.bin ens6 10032 127.0.0.1 10031 127.0.0.1
start-stop-daemon -S -b -n ens7 -x /home/mc36/pcapInt.bin ens7 10042 127.0.0.1 10041 127.0.0.1
start-stop-daemon -S -b -n ens8 -x /home/mc36/pcapInt.bin ens8 10052 127.0.0.1 10051 127.0.0.1
while (true); do
  /home/mc36/p4udp.bin 10.10.10.227 9080 0 127.0.0.1 127.0.0.1 10011 10012  10021 10022  10031 10032  10041 10042  10051 10052
  done
