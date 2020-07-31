#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop9- persist intop9.ini summary retry 16 url http://sources.nop.hu/cfg/ randord $1 $2 $3 $4 $5 $6 $7 $8
