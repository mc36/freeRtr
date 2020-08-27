#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
cd /home/mc36/rare
git pull
sudo dd if=/dev/zero of=/zzz bs=1M
sudo rm /zzz
