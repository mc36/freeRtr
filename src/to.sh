#!/bin/sh
java -Xmx512m -jar rtr.jar test tester opnflw- other opnflw.ini summary slot 121 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
./tm.sh rtropnflw-openflow- rtr2
