#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop2- persist intop2.ini summary slot 102 retry 8 url http://sources.freertr.org/cfg/ randord $@
./te.sh
./tm.sh rtrintop2- rtr10
