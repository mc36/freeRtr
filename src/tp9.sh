#!/bin/sh
./cn.sh
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang9.ini summary slot 1 parallel 60 retry 4 url http://sources.freertr.org/cfg/ $@
./te.sh
./tm.sh rtrp4lang-udp- rtr8
