#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get -t experimental install dpdk dpdk-dev openssl libssl-dev libpcap-dev
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
cd ~
mkdir a
wget -O a/rtr.zip http://src.mchome.nop.hu/rtr.zip
unzip a/rtr.zip -d a/
cd ~/a/misc/native
mkdir ../../binTmp/
./c.sh
mv ../../binTmp/p4*.bin ~/
cp ../p4emu/*.sh ~/
cd ~
rm -rf a/
