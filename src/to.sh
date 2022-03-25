#!/bin/sh
java -Xmx512m -jar rtr.jar test tester opnflw- other opnflw.ini summary slot 121 retry 16 url http://sources.freertr.net/cfg/ $@
./te.sh
