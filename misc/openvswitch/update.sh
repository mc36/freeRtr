#!/bin/sh
sudo apt update
sudo apt dist-upgrade
sudo apt remove apparmor
sudo apt autoremove
sudo apt clean
