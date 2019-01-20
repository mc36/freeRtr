#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop1 retry other ../../img/csr1kv.img virtio-net-pci 4096 randord $1 $2 $3 $4 $5 $6 $7 $8
