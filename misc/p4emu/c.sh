#!/bin/sh
apt-get install psmisc iproute2 net-tools socat gcc gdb llvm clang telnet libpcap-dev wget openssl libssl-dev dpdk dpdk-dev libbpf-dev bpftool tshark iperf
gcc -O3 -o dummyCon.bin ../native/dummyCon.c
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
