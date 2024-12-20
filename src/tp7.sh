#!/bin/sh
./cn.sh
ulimit -n 16384
java -Xmx512m -jar rtr.jar test tester p4lang- binary other p4lang5.ini slot 1 parallel 30 retry 4 $@
./te.sh
