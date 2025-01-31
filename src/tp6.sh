#!/bin/sh
java -Xmx256m -jar rtr.jar test image ../misc/image/platform.s390x ../misc/image/image.tst
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang6.ini slot 1 parallel 30 retry 4 $@
./te.sh
