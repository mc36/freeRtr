#!/bin/sh
java -Xmx256m -jar rtr.jar test tester opnflw- other opnflw.ini summary slot 121 retry 16 url http://sources.nop.hu/cfg/ $1 $2 $3 $4 $5 $6 $7 $8
