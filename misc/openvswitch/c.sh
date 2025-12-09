#!/bin/sh
apt-get install openvswitch-switch psmisc iproute2 net-tools socat gcc telnet tshark iperf
gcc -O3 -o dummyCon.bin ../native/dummyCon.c
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
