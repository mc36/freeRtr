#!/bin/sh
sudo wodim speed=2 -blank=fast
sleep 10
sudo wodim speed=2 -v ../../binImg/rtr-x86_64.iso
