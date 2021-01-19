#!/bin/sh
./c.sh
java -Xmx256m -jar rtr.jar test tester - summary slot 1 retry 16 url http://sources.nop.hu/cfg/ $@
