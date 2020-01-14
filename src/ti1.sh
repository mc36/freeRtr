#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop1- other ../../img/csr1kv.img virtio-net-pci 4096 summary retry url http://sources.nop.hu/cfg/ randord discard intop1-ike.* $1 $2 $3 $4 $5 $6 $7 $8
