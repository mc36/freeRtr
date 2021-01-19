#!/bin/sh
java -Xmx256m -jar rtr.jar test tester opnflw- other opnflw.ini summary slot 121 retry 16 url http://sources.nop.hu/cfg/ $@
