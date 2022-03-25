#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- other p4lang301.ini other p4lang302.ini other p4lang303.ini other p4lang304.ini other p4lang305.ini other p4lang306.ini other p4lang307.ini other p4lang308.ini other p4lang309.ini other p4lang310.ini other p4lang311.ini other p4lang312.ini other p4lang313.ini summary slot 146 paralell 10 retry 16 url http://sources.freertr.net/cfg/ $@
./te.sh
