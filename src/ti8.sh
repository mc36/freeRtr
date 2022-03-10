#!/bin/sh
java -Xmx512m -jar rtr.jar test tester intop8- other intop8.ini summary slot 108 retry 16 url http://sources.freertr.net/cfg/ $@
