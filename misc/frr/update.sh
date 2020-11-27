#!/bin/sh
sudo apt update
sudo apt dist-upgrade
sudo apt remove apparmor
sudo apt autoremove
sudo apt clean
sudo dd if=/dev/zero of=/zzz bs=1M
sudo rm /zzz
