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
cd ~/a/misc/p4lang
./c.sh
mv router.json ~/
mv router.txt ~/
cp *.sh ~/
cp *.py ~/
cp -r p4runtime_lib ~/
cd ~
rm -rf a/
sudo dd if=/dev/zero of=/zzz bs=1M
sudo rm /zzz
