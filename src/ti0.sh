#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop0- other intop0.ini summary slot 100 retry 8 url http://sources.freertr.org/cfg/ randord $@
killall -9 dynamips
./te.sh
