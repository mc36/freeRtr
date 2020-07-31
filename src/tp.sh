#!/bin/sh
java -Xmx256m -jar rtr.jar test tester p4lang- other p4lang.ini summary retry 16 url http://sources.nop.hu/cfg/ $1 $2 $3 $4 $5 $6 $7 $8
