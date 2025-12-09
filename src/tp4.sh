#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- other p4lang4.ini summary slot 158 parallel 10 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
./tm.sh rtrp4lang-xdp- rtr7
