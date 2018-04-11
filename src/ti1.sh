#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop1 retry other ../../img/csr1kv.img virtio 4096 $1 $2 $3 $4 $5 $6 $7 $8
