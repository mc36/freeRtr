#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
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
