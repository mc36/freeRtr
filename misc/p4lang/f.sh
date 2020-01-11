#!/bin/bash
sleep 5
while (true); do
  ./forwarder.py --p4info ./router.txt --bmv2-json ./router.json --p4runtime_address 127.0.0.1:50051 --freerouter_address 10.10.10.227 --freerouter_port 9080
  done
