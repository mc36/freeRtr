#!/bin/sh
./cn.sh
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang9.ini summary slot 1 paralell 60 retry 4 url http://sources.freertr.org/cfg/ $@
./te.sh
java -Xmx512m -jar rtr.jar test tstmov rtrp4lang-udp- rtr8
java -Xmx512m -jar rtr.jar test tstsum addone bmv2 rtr4 addone dpdk rtr5 addone tofino rtr6 addone xdp rtr7 addone udp rtr8
java -Xmx512m -jar rtr.jar test tstmov rtrp4sum- rtr3
