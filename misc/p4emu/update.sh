#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
cd ~
mkdir a
wget -O a/rtr.zip http://src.nop.hu/rtr.zip
unzip a/rtr.zip -d  a/
cd ~/a/misc/native
mkdir ../../binTmp/
./c.sh
mv ../../binTmp/p4*.bin ~/
cd ~
rm -rf a/
sudo dd if=/dev/zero of=/zzz bs=1M
sudo rm /zzz
