#!/bin/sh
./c.sh
java -Xmx512m -jar rtr.jar test tester - summary slot 1 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
