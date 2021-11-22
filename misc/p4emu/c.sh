#!/bin/sh
apt install psmisc iproute2 net-tools gcc gdb clang telnet libpcap-dev wget openssl libssl-dev dpdk dpdk-dev tshark iperf
gcc -O3 -o cons.bin cons.c
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
