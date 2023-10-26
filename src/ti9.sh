#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop9- persist intop9.ini summary slot 109 retry 8 url http://sources.freertr.org/cfg/ randord $@
./te.sh
./tm.sh rtrintop9- rtr12
