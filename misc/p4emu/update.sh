#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get -t experimental install linux-image-cloud-`dpkg --print-architecture` dpdk dpdk-dev openssl libssl-dev libpcap-dev libbpf-dev libxdp-dev liburing-dev libmnl-dev
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
rm p4*.bin
rm libp4*.so
cd ~
mkdir a
wget -O a/rtr.zip http://src.mchome.nop.hu/rtr.zip
unzip a/rtr.zip -d a/
cd ~/a/misc/native
mkdir ../../binTmp/
./c.sh
mv ../../binTmp/p4*.bin ~/
mv ../../binTmp/libp4*.so ~/
cp ../p4emu/*.sh ~/
cd ~
rm -rf a/
