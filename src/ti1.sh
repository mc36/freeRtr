#!/bin/sh
java -Xmx1024m -jar rtr.jar test tester intop1- persist intop1.ini summary discard intop1-ike.* slot 101 retry 8 url http://sources.freertr.org/cfg/ randord $@
./te.sh
./tm.sh rtrintop1- rtr9
