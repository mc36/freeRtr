#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop8- other intop8.ini summary slot 111 parallel 10 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
./tm.sh rtrintop8-frr- rtr11
