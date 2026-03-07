#!/bin/sh
apt-get install openvswitch-switch psmisc iproute2 net-tools socat gcc telnet tshark iperf
gcc -O3 -o dummyCon.bin ../native/dummyCon.c
cp initd /etc/init.d/rtr
chmod 755 /etc/init.d/rtr
update-rc.d rtr defaults
cp service1 /lib/systemd/system/rtr.service
cp service2 /lib/systemd/system/dmp.service
systemctl daemon-reload
systemctl unmask rtr
systemctl enable rtr
systemctl unmask dmp
systemctl enable dmp
