#!/bin/sh
sudo apt update
sudo apt dist-upgrade
sudo apt remove apparmor
sudo apt autoremove
sudo apt clean
cd ~
mkdir a
wget -O a/rtr.zip src.mchome.nop.hu/rtr.zip
unzip a/rtr.zip -d a/
sudo chown -R mc36:mc36 *
rm -rf rare/profiles/
rm -rf rare/p4src/include/
rm -rf rare/bfrt_python/rare/
mv a/misc/p4bf/snmp/* rare/snmp/
mv a/misc/p4bf/profiles rare/
mv a/misc/p4bf/*.p4 rare/p4src/
mv a/misc/p4bf/include rare/p4src/
mv a/misc/p4bf/*.py rare/bfrt_python/
mv a/misc/p4bf/rare rare/bfrt_python/
mv a/misc/p4bf/*.sh ./
cd ~
rm -rf a/
sudo rm `find . -name *.log*`
sudo rm `find . -name *.pcap`
