#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop8- other ../../img/frr.img e1000 1024 summary retry url http://sources.nop.hu/cfg/ randord $1 $2 $3 $4 $5 $6 $7 $8
