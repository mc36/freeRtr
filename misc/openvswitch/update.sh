#!/bin/sh
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get remove apparmor
sudo apt-get autoremove
sudo apt-get clean
