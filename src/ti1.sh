#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop1- persist intop1.ini discard .*ike.* summary slot 101 retry 16 url http://sources.freertr.net/cfg/ randord $@
./te.sh
