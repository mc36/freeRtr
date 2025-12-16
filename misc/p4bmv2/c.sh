#!/bin/sh
#wget -O /etc/yum.repos.d/p4lang.repo https://copr.fedorainfracloud.org/coprs/nucleo/p4lang/repo/fedora-rawhide/nucleo-p4lang-fedora-rawhide.repo
#sudo dnf install p4lang-pi p4lang-p4c p4lang-bmv2 tshark iperf gcc git telnet
#deb http://download.opensuse.org/repositories/home:/p4lang/Debian_11/ /
#wget https://download.opensuse.org/repositories/home:p4lang/Debian_11/Release.key
#sudo apt-key add ./Release.key
#sudo apt-get install p4lang-pi p4lang-p4c p4lang-bmv2 psmisc gpg iproute2 net-tools tshark iperf gcc git telnet
#git clone git@github.com:rare-freertr/RARE-bmv2
#gcc -O3 -o dummyCon.bin ../native/dummyCon.c
#cp initd /etc/init.d/rtr
#chmod 755 /etc/init.d/rtr
#update-rc.d rtr defaults
p4c-bm2-ss --Wwarn --std p4-16 --target bmv2 --arch v1model --p4runtime-files router.txt -o router.json router.p4
