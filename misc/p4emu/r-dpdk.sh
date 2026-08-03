#!/bin/sh
echo 1 > /proc/sys/net/ipv6/conf/all/disable_ipv6
echo 64 > /proc/sys/vm/nr_hugepages
modprobe uio_pci_generic
dpdk-devbind.py -b uio_pci_generic 00:04.0
dpdk-devbind.py -b uio_pci_generic 00:05.0
dpdk-devbind.py -b uio_pci_generic 00:06.0
dpdk-devbind.py -b uio_pci_generic 00:07.0
dpdk-devbind.py -b uio_pci_generic 00:08.0
while (true); do
#  /home/mc36/p4dpdk.bin -- 10.10.10.227 9080 0
#  /home/mc36/p4dpdk.bin -- 10.10.10.227 9080 0    -1 1 2
  /home/mc36/p4dpdk.bin -- 10.10.10.227 9080 0    0 0 0   1 1 1   2 0 0    3 1 1   4 0 0
  done
