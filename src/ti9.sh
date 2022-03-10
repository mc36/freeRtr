#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop9- persist intop9.ini summary slot 109 retry 16 url http://sources.freertr.net/cfg/ randord $@
