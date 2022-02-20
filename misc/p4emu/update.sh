#!/bin/sh
sudo apt update
sudo apt dist-upgrade
sudo apt remove apparmor
sudo apt autoremove
sudo apt clean
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
