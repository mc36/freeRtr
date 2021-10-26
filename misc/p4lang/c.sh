#!/bin/sh
#deb http://download.opensuse.org/repositories/home:/frederic-loui:/p4lang:/p4c:/master/Debian_10/ ./
#wget https://download.opensuse.org/repositories/openSUSE:/Tools/Debian_10/Release.key
#wget https://download.opensuse.org/repositories/home:/frederic-loui:/p4lang:/p4c:/master/Debian_10/Release.key
#wget https://download.opensuse.org/repositories/home:/frederic-loui:/p4lang:/p4c/Debian_10/Release.key
#sudo apt-key add ./Release.key
#sudo apt install p4c bmv2 psmisc iproute2 net-tools tshark iperf gcc telnet
#git clone git@github.com:frederic-loui/RARE.git
#gcc -O3 -o cons.bin cons.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
p4c-bm2-ss --Wwarn --std p4-16 --target bmv2 --arch v1model --p4runtime-files router.txt -o router.json router.p4
