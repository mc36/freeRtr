#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- other p4lang4.ini summary slot 158 paralell 10 retry 16 url http://sources.freertr.net/cfg/ $@
./te.sh
