#!/bin/sh
./c.sh
./cb.sh
java -Xmx512m -jar rtr.jar test tester - binary summary slot 1 retry 16 url http://sources.nop.hu/cfg/ $@
