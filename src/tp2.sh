#!/bin/sh
java -Xmx512m -jar rtr.jar test tester p4lang- other p4lang2.ini summary slot 134 paralell 10 retry 16 url http://sources.nop.hu/cfg/ $@
