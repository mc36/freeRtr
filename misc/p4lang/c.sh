#!/bin/sh
#deb http://download.opensuse.org/repositories/home:/rstoyanov/Debian_11/ /
#wget https://download.opensuse.org/repositories/home:rstoyanov/Debian_11/Release.key
#sudo apt-key add ./Release.key
#sudo apt install p4lang-pi p4lang-p4c p4lang-bmv2 psmisc gpg iproute2 net-tools tshark iperf gcc git telnet
#git clone git@github.com:frederic-loui/RARE.git
#gcc -O3 -o cons.bin cons.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
p4c-bm2-ss --Wwarn --std p4-16 --target bmv2 --arch v1model --p4runtime-files router.txt -o router.json router.p4
