#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
sudo dd if=/dev/zero of=/zzz.bin bs=1M
sudo rm /zzz.bin
