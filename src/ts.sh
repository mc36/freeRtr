#!/bin/sh
java -Xmx512m -jar rtr.jar test tstsum addone udp rtr8 addone dpdk rtr5 addone xdp rtr7 addone bmv2 rtr4 addone tofino rtr6
./tm.sh rtrp4sum- rtr3
