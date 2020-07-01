#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop8- other intop8.ini summary retry url http://sources.nop.hu/cfg/ randord $1 $2 $3 $4 $5 $6 $7 $8
