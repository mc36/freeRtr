#!/bin/sh
#sudo apt install bf_sde psmisc iproute2 net-tools socat tshark iperf gcc telnet
#git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare.git
#git clone ssh://git@bitbucket.software.geant.org:7999/rare/rare-bf2556x-1t.git
#gcc -O3 -o cons.bin cons.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
#systemctl mask serial-getty@ttyS0
#systemctl disable serial-getty@ttyS0
#echo mc36 ALL=(ALL) NOPASSWD:ALL >> /etc/sudoers
cd /home/mc36/rare/p4src
export SDE=/home/mc36/bf-sde-9.3.0
export SDE_INSTALL=/home/mc36/bf-sde-9.3.0/install
sudo -E $SDE/tools/p4_build.sh -I. $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $16 $17 $18 $19 ./bf_router.p4
