#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang9.ini summary slot 1 paralell 60 retry 16 url http://sources.freertr.org/cfg/ $@
./te.sh
