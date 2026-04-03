#!/bin/sh
java -Xmx1024m -jar rtr.jar test tester $1 wait $@
