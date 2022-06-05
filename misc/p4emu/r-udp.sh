#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
ip link set ens4 up promisc on mtu 8192
ip link set ens5 up promisc on mtu 8192
ip link set ens6 up promisc on mtu 8192
ip link set ens7 up promisc on mtu 8192
ip link set ens8 up promisc on mtu 8192
start-stop-daemon -S -b -x /home/mc36/r-ens4.sh
start-stop-daemon -S -b -x /home/mc36/r-ens5.sh
start-stop-daemon -S -b -x /home/mc36/r-ens6.sh
start-stop-daemon -S -b -x /home/mc36/r-ens7.sh
start-stop-daemon -S -b -x /home/mc36/r-ens8.sh
while (true); do
  /home/mc36/p4udp.bin 10.10.10.227 9080 0 127.0.0.1 127.0.0.1  10012 10011  10022 10021  10032 10031  10042 10041  10052 10051
  done
