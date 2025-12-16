#!/bin/sh
java -Xmx512m -jar rtr.jar test tester $1 wait $@
