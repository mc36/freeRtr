#!/bin/sh
apt-get install openvswitch-switch socat gcc telnet tshark iperf
gcc -O3 -o cons.bin cons.c
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
