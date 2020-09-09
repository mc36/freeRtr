#!/bin/sh
#sudo apt-get install bf_sde socat tshark iperf gcc telnet
#gcc -O3 -o cons.bin cons.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
#systemctl mask serial-getty@ttyS0
#systemctl disable serial-getty@ttyS0
export SDE=/usr/share/bf_switchd
export SDE_INSTALL=/usr/share/bf_switchd/install
cd /home/mc36/rare/p4src
sudo -E $SDE/tools/p4_build.sh -I. $1 $2 $3 $4 $5 $6 $7 $8 ./bf_router.p4
