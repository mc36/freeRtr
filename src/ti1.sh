#!/bin/sh
java -Xmx256m -jar rtr.jar test tester intop1- persist intop1.ini summary slot 101 retry 16 url http://sources.nop.hu/cfg/ randord $1 $2 $3 $4 $5 $6 $7 $8
