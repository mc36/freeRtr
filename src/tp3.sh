#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- other p4lang301.ini other p4lang302.ini other p4lang303.ini other p4lang304.ini other p4lang305.ini other p4lang306.ini other p4lang307.ini other p4lang308.ini other p4lang309.ini other p4lang310.ini other p4lang311.ini other p4lang312.ini other p4lang313.ini other p4lang314.ini other p4lang315.ini other p4lang316.ini other p4lang317.ini summary slot 146 parallel 10 retry 8 url http://sources.freertr.org/cfg/ $@
./te.sh
./tm.sh rtrp4lang-tofino- rtr6
