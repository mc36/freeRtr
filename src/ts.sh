#!/bin/sh
java -Xmx512m -jar rtr.jar test tstsum addone udp rtr8 addone bmv2 rtr4 addone dpdk rtr5 addone tofino rtr6 addone xdp rtr7
java -Xmx512m -jar rtr.jar test tstmov rtrp4sum- rtr3
