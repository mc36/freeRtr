#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop2- persist intop2.ini summary slot 102 retry 16 url http://sources.nop.hu/cfg/ randord $@
