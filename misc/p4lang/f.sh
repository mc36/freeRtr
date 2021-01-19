#!/bin/sh
###sleep 5
cd /home/mc36
while (true); do
  ./forwarder.py --p4info ./router.txt --bmv2-json ./router.json --freerouter_address 10.10.10.227
  done
